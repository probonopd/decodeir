#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <set>
#include <iostream> 

#include "DecodeIR.h"

#include<assert.h>
#define __cdecl
#define _ASSERT assert
#define DWORD int
#define LPVOID void *
#define strnicmp strncasecmp


static char const version_cstr[]="2.45";	

Signal::Signal(
		unsigned int* p_Context,
		int* p_Bursts,
		int n_Freq,
		int n_Single,
		int n_Repeat,
		char* p_Protocol,
		int* p_Device,
		int* p_SubDevice,
		int* p_OBC,
		int* p_Hex,
		char* p_Misc,
		char* p_Error) :
	pContext(p_Context),
	nFreq_in(n_Freq),
	nSingle(n_Single),
	nRepeat(n_Repeat),
	pProtocol(p_Protocol),
	pDevice(p_Device),
	pSubDevice(p_SubDevice),
	pOBC(p_OBC),
	pHex(p_Hex),
	pMisc(p_Misc),
	pError(p_Error),
	pDuration(NULL)
{
	// GD 2009 Start
	nFrameCount = 0;
	nDittos = 0;
	nNote_out = -1;		// unset
	nAuxNote_out = -1;	// unset
	nNonSpurious = 0;	// Only valid when >0
	bXMPHalfValid = false;
	AminoToggleDetected = false; 
	ZaptorToggleDetected = false;
	bInitLeadIn = false;
	// Don't output frame count if repeat or extra frames present.  If neither are present
	// then *pDevice = -2 can still be used to suppress frame count, as *pDevice is only
	// taken as carrying nExtra if nRepeat is nonzero.
	bSuppressCount = nRepeat > 0 || *pDevice < -1;
	// Older versions of IR.exe add extra count to single count when no repeat frames.
	// This is retained for compatibility, so a value of *pDevice , -1 is ignored unless
	// nRepeat > 0.
	nExtra = nRepeat > 0 && *pDevice < -1 ? - *pDevice : 0;
	// Additional info can be provided or returned in Context[] if its length is carried in 
	// pSubDevice and is > 2.  The maximum supported length is 18, due to the form of the
	// returned handshake.  See decode2() for details of the use of the additional fields.
	nContextLength = *pSubDevice < -1 ? - *pSubDevice : 2;
	// The only additional incoming data in an enhanced context is a pointer, potentially 64-bit,
	// to an array of carrier cycle counts corresponding to the timings in p_Bursts[].  This is
	// only used by IRScope, which uses nRepeat=nExtra=0, so it does not need extending in the
	// way that pDuration[] is extended from p_Bursts.
	pCounts = nContextLength > 7 ? reinterpret_cast<short int*>(*(int64_t *)(&pContext[6])) : NULL;
	// GD 2009 End

	int nSR = (nRepeat+nSingle)*2;
//	pDuration = new float[nRepeat*2 + nSR + 4];
	int nSRE = nSR + nExtra*2;	// GD 2009
	pDuration = new float[nRepeat*2 + nSRE + 4];		// GD 2009
	for (int ndx=0; ndx<nSR; ++ndx)
	{
		pDuration[ndx] = p_Bursts[ndx];
	}
	memcpy(pDuration+nSR, pDuration+2*nSingle, nRepeat*2*sizeof(float));
	// GD 2009 Start
	for (int ndx=nSR; ndx<nSRE; ++ndx)
	{
		pDuration[ndx+2*nRepeat] = p_Bursts[ndx];
	}
	// GD 2009 End
	// memset(pDuration+nSR+2*nRepeat, 0, 4*sizeof(float));
	memset(pDuration+nSRE+2*nRepeat, 0, 4*sizeof(float));	// GD 2009

	// GD 2009  Start.  If nExtra > 0 treat all bursts as single
	if ( nExtra > 0 )
	{
		nNonSpurious = nSingle + 2*nRepeat;	// Extra bursts can be spurious
		nSingle += 2*nRepeat + nExtra;
		nRepeat = 0;
		nExtra = 0;
	}
	// GD 2009 End

	if ( nRepeat==0 )
		pDuration[nSingle*2-1] = 999999.; 
	pSuffix = new char[16];
	strcpy(pSuffix, "" );
}
Signal::~Signal()
{
	delete pDuration;
	delete pSuffix;
}

void Signal::setPreempt(int prValue)
// GD 2009.  This sets a preempt value when there is a valid decode which
// is not reported at the time, so we could not use NewPreemptValue as
// that only takes effect when protocol is reported.
{
	if (	preemptValue < prValue	
		||	preemptValue == prValue	
			&&	nFrameL > preemptLength )	
	{										
		preemptValue = prValue;
		preemptLength = nFrameL;
	}
}

void Signal::setzContext()
// GD 2009.  This sets the zContext value, which records pContext array from the
// start of a repeat sequence, and is used when that start is suppressed by the
// decoder.
{
	int nStart = (pFrame - pDuration)/2;
	zContext[0] = nStart + (preemptLength<<20);
	// nProtocol is incremented before being stored in pContext[1]
	zContext[1] = ((nFrameL - MinFrame)<<16) + (preemptValue<<8) + nProtocol + 1;
}

void Signal::decode()
{
	typedef void (Signal::* SignalFnc)();
	static const SignalFnc Funcs[] = {
		&Signal::tryXMP,
		&Signal::tryZenith,
		&Signal::tryX10,
		&Signal::tryXX,
		&Signal::tryTDC,		// GD 2009. Needs to be above Q2
		&Signal::trySejin,		// GD 2009
		&Signal::tryQ1,
		&Signal::tryQ2,
		&Signal::tryDirecTV,
		&Signal::trySomfy,
		&Signal::tryGap,		// if this moves, change nGapProtocol value	
		&Signal::tryRC5,
		&Signal::tryRC6,
		&Signal::tryCanalSat,	// GD 2009
		&Signal::tryNokia,
		&Signal::tryPCTV,
		&Signal::tryF12,
		&Signal::trySony,
		&Signal::trySingleBurstSize,
		&Signal::tryGXB,
		&Signal::tryAsync,
		&Signal::tryAirboard,
		&Signal::tryAirAsync,
		&Signal::tryAK,
		&Signal::trySunfire,
		&Signal::tryGrundig16,
		&Signal::tryPid13,
		&Signal::tryLutron,		// GD 2010
		&Signal::tryElan,		// DAR 2012
		&Signal::tryBryston,	// DAR 2012
		&Signal::tryHumax,		// DAR 2012
		&Signal::tryAdNotam		// DAR 2012
	};

	nGapProtocol = 10;

	*pProtocol = 0;
	// GD 2009 Start - Initialize output variables as IRScope does not appear
	// to do so on repeat calls
	/* if ( *pDevice >= 0 ) */ *pDevice = -1;	// Initialize Device only if not carrying ExtraBurstCount
	*pSubDevice = -1;
	*pOBC = -1;
	for (int n = 0; n < 4; n++) pHex[n] = -1;
	*pMisc = 0;
	*pError = 0;
	// GD 2009 End
	int nStart = pContext[0] & 0xFFFFF;
	preemptLength = pContext[0] >> 20;
	nFrameL = (pContext[1] >> 16) + MinFrame;
	nProtocol = (pContext[1] & 0xFF);
	preemptValue = (pContext[1] >> 8) & 0xFF;
	newPreemptValue = 0;
	newPreemptLength = 0;

	int nStartLimit = nSingle + nRepeat;
	int nTot = nStartLimit + nRepeat;
	pMainLimit = pDuration + 2 * nStartLimit;
	pFullLimit = pMainLimit + 2 * nRepeat;
	if (nRepeat < MinFrame)
		nStartLimit = nTot - (MinFrame-1);

 // /* Cover bug in CCF2EFC */ if (nStartLimit>200) nStartLimit=200;

	for (; nStart<nStartLimit; nStart++)
	{
		pFrame = pDuration + nStart*2;
        frameLeft = 1e9;
        if ( nStart )
        {
            frameLeft = pFrame[-1];
            if ( nStart == nSingle )
            {
                float wrap = pDuration[ 2*(nSingle+nRepeat) -1 ];
                if ( wrap >= frameLeft )
                    frameLeft = wrap;
            }
        }
		if ( nStart == 0 || nStart == nSingle || pFrame[0]+pFrame[1] < pFrame[-2]*.5+pFrame[-1]
			|| pFrame[0] > 14000 )  // This last one deals with Lutron, which uses a long lead-in rather than framing
		{
			pFrameEnd = pFrame + nFrameL*2-3;	// Point to Off half of second to last burst (advanced later to be final Off)
			nTotDur = 0;
			for (float* pDur = pFrame; ; )
			{
				sortOn.insert(pDur);
				sortOff.insert(pDur+1);
				sortBurst.insert1(pDur);
				nTotDur += pDur[0] + pDur[1];
				pDur+=2;
				if (pDur >= pFrameEnd)
					break;
			}
            sortBurst.findMid();

			int nMaxL = nTot - nStart;  // Maximum frame length that could start at nStart
			if (nMaxL > MaxFrame)
				nMaxL = MaxFrame;
			for (; ; nFrameL++)  //  Loop over posible frame lengths
			{
				pFrameEnd++;    // Point at last burst (lead out)
				sortOn.insert(pFrameEnd);

				nMaxDur = sortOff.max1;   // Excludes final Off
                if (nMaxDur < sortOn.max1)
					nMaxDur = sortOn.max1;

				nTotDur += *pFrameEnd++;    // On
                
                frame = *pFrameEnd;
                if ( frameLeft <= frame )
                    frame = frameLeft;

				// GD Start


				int xxStart = nStart;
				int xxFrameL = nFrameL;
				// Calc freq for this frame
				nFreq = (nContextLength > 4 && pCounts != NULL) ? getFreq(nStart, nStart+nFrameL) : nFreq_in;
				// GD End

				for ( ;nProtocol < sizeof(Funcs)/sizeof(*Funcs); )
				{
					(this->*(Funcs[nProtocol]))();
					++nProtocol;
					if (*pProtocol)
					{
						if (   newPreemptValue > preemptValue
							||    newPreemptValue == preemptValue
							   && newPreemptLength > preemptLength )
						{
							preemptValue = newPreemptValue;
							preemptLength = newPreemptLength;
						}
						pContext[0] = nStart + (preemptLength<<20);
						pContext[1] = ((nFrameL - MinFrame)<<16) + (preemptValue<<8) + nProtocol;
						return;
					}
				}
				nProtocol = 0;

                if ( nFrameL >= nMaxL )
                    break;

				nTotDur += *pFrameEnd;    // Off
				sortBurst.insert(pFrameEnd-1);
				sortOff.insert(pFrameEnd);
			}	// for (; ; nFrameL++)
			sortOn.clear();
			sortOff.clear();
			sortBurst.clear();
		}	// if ( nStart == 0 || ...
		nFrameL=MinFrame;
		if (--preemptLength <= 0)
		{
			preemptLength = preemptValue = 0;
		}
	}  // for (; nStart<nStartLimit; nStart++)
}

void Signal::decode2()
{
	decode();

	int xDevice = *pDevice;
	int xSubDevice = *pSubDevice;
	int xOBC = *pOBC;
	int xHex[4];
	unsigned int wContext[2];	// the pContext for the next iteration
	unsigned int xContext[2];	// the pContext of the first decode of a repeat sequence
	unsigned int yContext[2];	// the pContext of the last decode of a repeat sequence
	int xNote_out = nNote_out;
	int xAuxNote_out = nAuxNote_out;
	int contextLengthOut = 2;
	int xFrameCount;
	int xDittos;
	int xInitLeadIn = bInitLeadIn;
	int n;
	int bSetyContext = true;
	// Test if lead-in only with tryGap, note that nProtocol is already incremented
	int xLeadIn = ( ((pContext[1] & 0xFF) == nGapProtocol + 1) && (pLead) ) ? 1 : 0;
	int yLeadIn;
	char xProtocol[256];
	char xMisc[256];
	int iEndpoint = 0;	// -2 & -1 = saved 1 & 0; 0 = no early end; 1 = early end
	for (n = 0; n < 4; n++) xHex[n] = pHex[n];
	for (n=0; n<2; n++) xContext[n] = nFrameCount == 0 ? pContext[n] : zContext[n];
	strcpy(xProtocol, pProtocol);
	strcpy(xMisc, pMisc);
	if ( *pError == 0 && *pProtocol )
	{
		do
		{
			do	
			{
				if (bSetyContext)
				{
					// yContext is not set if the most recent decode has been suppressed as spurious
					for (n=0; n<2; n++) yContext[n] = pContext[n];	// save the current sequence end
				}
				for (n=0; n<2; n++) wContext[n] = pContext[n];		// save for next iteration
				bSetyContext = true;
				xFrameCount = ++nFrameCount;	// count the frame just validated
				xDittos = nDittos;				// save the ditto count so not upset by next decode
				iEndpoint = ( iEndpoint < 0 ) ? - iEndpoint - 1 :	// if <0 restore old value, else test
					(	nDittos != 0			// don't look for repeats if dittos present
					||	*pFrameEnd > 200000 );	// don't look for repeats if more than 200ms between frames
				sortOn.clear();
				sortOff.clear();
				sortBurst.clear();
				decode();						// check for spurious next frame even if early end
				yLeadIn = ( ((pContext[1] & 0xFF) == nGapProtocol + 1) && (pLead) ) ? 1 : 0;
			}
			while (	*pError == 0	
				&&	strcmp(pProtocol, xProtocol) == 0
				&&	*pDevice == xDevice
				&&	*pSubDevice == xSubDevice
				&&	*pOBC == xOBC
				&&	strcmp(pMisc, xMisc) == 0
				&&	nDittos == 0		// if new frame has dittos, or nDittos=-1, treat as non-matching
				&&	!bInitLeadIn		// if new frame has initial lead-in, treat as non-matching
				&&	(yContext[0] & 0xFFFFF) + (yContext[1] >> 16) + MinFrame
						== (pContext[0] & 0xFFFFF) - yLeadIn	// if not consecutive, treat as non-matching
				&&	!iEndpoint);		// exit loop if early end marked

			// If a generic Gap decode is followed by a real decode starting one burst later,
			// Gap decode was probably spurious, treating a real lead-in as start of data.
			// So cancel the Gap decode, remove it from the count and reset the 'x...' variables
			if (	*pError == 0 && *pProtocol
				&&	strncmp(xProtocol, "Gap-", 4) == 0
				&&	strncmp(pProtocol, "Gap-", 4) != 0
				&&	((pContext[0] - yContext[0]) & 0xFFFFF) == 1 )	// check start difference is 1 burst
			{
				--nFrameCount;			// remove from count
				iEndpoint = 0;			// no early end to count, but let the real decode be tested
				xDevice = *pDevice;
				xSubDevice = *pSubDevice;
				xOBC = *pOBC;
				xNote_out = nNote_out;
				xAuxNote_out = nAuxNote_out;
				xLeadIn = ( ((pContext[1] & 0xFF) == nGapProtocol + 1) && (pLead) ) ? 1 : 0;
				for (n = 0; n < 4; n++) xHex[n] = pHex[n];
				for (n=0; n<2; n++) xContext[n] = pContext[n];
				strcpy(xProtocol, pProtocol);
				strcpy(xMisc, pMisc);
			}
			else
			// If a decode is followed by a generic Gap decode with same start frame or one burst,
			// later, Gap decode was probably spurious, treating two or more real frames as one.
			// So skip the Gap decode and remove it from the count
			if (	*pError == 0 && *pProtocol
				&&	strncmp(pProtocol, "Gap-", 4) == 0
				&&	((pContext[0] - yContext[0]) & 0xFFFFE) == 0 )	// check start burst
			{
				--nFrameCount;					// remove spurious Gap decode from count
				bSetyContext = false;			// do not set yContext from it
				iEndpoint = - iEndpoint - 1;	// allow to continue but save old value
			}
			else
			// If a decode is followed by a Solidtek decode other than the documented ones of
			// Solidtek16 and Solidtek20, or by an S:xx.xx... decode (all these in tryQ2)
			// and the whole of this following decode frame lies within the frame of the
			// previous decode, then this second decode is probably spurious.  So skip
			// it and remove it from the count.
			if (	*pError == 0 && *pProtocol
				&&	(	strncmp(pProtocol, "S:", 2) == 0
					||	(	strncmp(pProtocol, "Solidtek", 8) == 0
						&&	strncmp(pProtocol+8, "16", 2) != 0
						&&	strncmp(pProtocol+8, "20", 2) != 0 ) )
				&&	( (pContext[0] & 0xFFFFF) + (pContext[1] >> 16)	// compare nStart + nFrameL
						<= (yContext[0] & 0xFFFFF) + (yContext[1] >> 16) ) )
			{
				--nFrameCount;					// remove spurious Solidtek decode from count
				bSetyContext = false;			// do not set yContext from it
				iEndpoint = - iEndpoint - 1;	// allow to continue but save old value
			}
			else
			// combine two types of X10 frame
			if (	*pError == 0 
				&&	strcmp(pProtocol, "X10") == 0
				&&	strncmp(xProtocol, "X10", 3) == 0
				&&	xOBC == *pOBC 
				&&	xHex[0] == pHex[0] )
			{
				if ( xInitLeadIn )
				{
					xNote_out = 4;
					xAuxNote_out = -1;
					*xMisc = 0;				// cancel the "invalid signal"
					xInitLeadIn = false;	// only needed once
				}
			}
			else
			// If a Solidtek16 or Solidtek20 decode is followed by various decodes
			// of fewer frames in which the first 8 bits match the Solidtek data then
			// it is probably a spurious decode of a partial Solidtek repeat at the end
			// of a signal.  Skip the spurious decode and remove it from the count.
			if (	*pError == 0
				&&	(	strncmp(pProtocol, "S:", 2) == 0
					||	strcmp(pProtocol, "Blaupunkt{prefix}") == 0 
					||	strcmp(pProtocol, "XX") == 0
					||	strcmp(pProtocol, "DirecTV") == 0)
				&&	(	strcmp(xProtocol, "Solidtek16") == 0
					||	strcmp(xProtocol, "Solidtek20") == 0 ) )	
			{
				int x2;
				int xbits;
				char xstr[3];

				// reconstruct first 8 bits of Solidtek decode as hex string in xstr
				if ( strcmp(xProtocol+8, "16") == 0 ) x2 = xOBC;
				else x2 = xSubDevice;
				xbits = ( msb(xDevice,4)<<3 |  msb(x2,3) ) & 0xFF;
				sprintf(xstr, "%02X", xbits);

				// check those that can be checked with this xbits value
				if (		strcmp(pProtocol, "Blaupunkt{prefix}") == 0
						&&	strcmp(xstr, "00") == 0	// needed to match Blaupunkt
					||		strcmp(pProtocol, "DirecTV") == 0
						&&	*pDevice == 0 && *pOBC == 0
						&&	strcmp(xstr, "00") == 0	// needed to match DirecTV
					||		strncmp(pProtocol, "S:", 2) == 0
						&&	strncmp(pProtocol+5, xstr, 2 ) == 0 )	// test against yy of S.xx.yy....
				{
					--nFrameCount;					// remove spurious decode from count
					bSetyContext = false;			// do not set yContext from it
					iEndpoint = - iEndpoint - 1;	// allow to continue but save old value
				}
				else
				// check the one that needs complemented xbits value
				{	sprintf(xstr, "%02X", 255-xbits);
					if (	strcmp(pProtocol, "XX") == 0
						&&	strncmp(pMisc, xstr, 2 ) == 0 )
					{
						--nFrameCount;					// remove spurious decode from count
						bSetyContext = false;			// do not set yContext from it
						iEndpoint = - iEndpoint - 1;	// allow to continue but save old value
					}
					else break;
				}
			}
			else
			// The Sony protocol can match the Async protocol if the Sony data comprises
			// binary pairs 01 and 10.  The Sony lead-in translates to Async 0x80, the
			// binary pairs 01 and 10 translate to Async 0xE6 and 0x98.  The minimum
			// required for an Async decode is 3 such Sony binary pairs.  So if a Sony
			// decode is followed by such an Async decode then the Async probably comes
			// from a truncated Sony frame. Skip the spurious decode and remove it from
			// the count. 
			if (	*pError == 0
				&&	strncmp(xProtocol, "Sony", 4) == 0
				&&	strncmp(pProtocol, "Async", 5) == 0 
				&&	strncmp(pMisc, "80", 2) == 0
				&&	(	strncmp(pMisc+3, "98", 2) == 0
					||	strncmp(pMisc+3, "E6", 2) == 0 )
				&&	(	strncmp(pMisc+6, "98", 2) == 0
					||	strncmp(pMisc+6, "E6", 2) == 0 )
				&&	(	strncmp(pMisc+9, "98", 2) == 0
					||	strncmp(pMisc+9, "E6", 2) == 0 ) )
			{
				--nFrameCount;					// remove spurious Async decode from count
				bSetyContext = false;			// do not set yContext from it
				iEndpoint = - iEndpoint - 1;	// allow to continue but save old value
			}
			else
			// If an XMP decode with valid first half frame is followed by a Mitsubishi decode with
			// same start frame, Mitsubishi decode was probably a spurious decode of an XMP signal
			// with one or more missing digits (Mitsubishi frame length is 17, valid XMP is 18).
			// So skip the Mitsubishi decode and remove it from the count.  If counts suppressed,
			// i.e. learning remotes rather than IRScope, do same if following decode is consecutive
			// and is another XMP frame.
			if (	*pError == 0 && *pProtocol
				&&  (strcmp(xProtocol, "XMP") == 0 || strncmp(xProtocol, "XMP-", 4) == 0)
				&&	(strcmp(pProtocol, "Mitsubishi") == 0
					&&	((pContext[0] - yContext[0]) & 0xFFFFF) == 0 	// check start burst
				||	strncmp(pProtocol, "XMP", 3) == 0 
					&& (yContext[0] & 0xFFFFF) + (yContext[1] >> 16) + MinFrame
						== (pContext[0] & 0xFFFFF) 
					&& bSuppressCount )
				&&  bXMPHalfValid )
			{
				// char xstr[100];
				// sprintf(xstr, "start1 = %d start2 = %d",(yContext[0] & 0xFFFFF) + (yContext[1] >> 16) + MinFrame,(pContext[0] & 0xFFFFF));
				// MessageBox(0,xstr,"Error",0);
				if (strcmp(pProtocol, "Mitsubishi") == 0)
				{	
					--nFrameCount;					// remove spurious Mitsubishi decode from count
					bSetyContext = false;			// do not set yContext from it
				}
				iEndpoint = - iEndpoint - 1;	// allow to continue but save old value
			}
			else break;
		}
		while ( iEndpoint <= 0 );

		// Frame just decoded does not match earlier frame.  If it comes from
		// extra bursts, treat it as spurious so prevent its decode
		if (	nNonSpurious > 0		// value has been set
			&&	(int)(pContext[0] & 0xFFFFF) >= nNonSpurious	)
		{
			wContext[0] = 0xFFFFF;
		}		
		if  	(	strcmp(xProtocol, "NEC") == 0
				||	strcmp(xProtocol, "NECx") == 0 
				||	strcmp(xProtocol, "48-NEC") == 0 )
		{
			if (xFrameCount > 1 || nDittos == 0 && nRepeat > 6 )  
			{							 //nRepeat > 0 changed to nRepeat > 6  DAR Dec 2010 to avoid detecting faulty 
				strcat(xProtocol, "2");  //ditto(s) as NEC2. Anyway, NEC2 should have nRepeat == 34 if xFrame == 1
			}  
			strcat (xProtocol,pSuffix);   //DAR Nov 2010 
		}
		else if (	strcmp(xProtocol, "JVC") == 0 
				&&	!xInitLeadIn )
		{
			strcat(xProtocol, "{2}");
			xNote_out = 1;
		}
		else if (	strncmp(xProtocol, "RCA", 3) == 0 
				&&	xInitLeadIn )
		{
			strcat(xProtocol, "(Old)");
			xNote_out = 11;
		}
		else if (	strncmp(xProtocol, "Gap-", 4) == 0
				||	strncmp(xProtocol, "??", 2) == 0 )
		{
			xNote_out = 8;
		}

		// Restore pContext to value saved before last call to decode()
		for (n=0; n<2; n++) pContext[n] = wContext[n];

		int iParam = xFrameCount;

		*pDevice = xDevice;
		*pSubDevice = xSubDevice;
		*pOBC = xOBC;
		for (n=0; n < 4; n++) pHex[n] = xHex[n];
		strcpy(pProtocol, xProtocol);
		if (xNote_out == -1)		// if note unset, set default
		{
			if (xDittos > 0)		// if dittos present
			{
				xNote_out = 3;		// set ditto-frame note
				iParam = xDittos;
			}
			else
			{
				xNote_out = 1;		// else set identical repeats note
			}
		}

		if (nContextLength > 2)		// extended context used by IRScope
		{
			// set pContext[2] to the pContext[0] value at start of repeat series
			// but without its preemptLength value as that is no longer relevant
			pContext[2] = xContext[0] & 0xFFFFF;
			// if there was a tryGap() lead-in then actual start is one frame earlier
			if ( pContext[2] > 0 ) pContext[2] -= xLeadIn;
			// carry the preemptLength for the next iteration in pContext[2] and mask
			// it out of pContext[0] as that field of pContext[0] is used for the handshake
			pContext[2] |= pContext[0] & 0xFFF00000;
			pContext[0] &= 0xFFFFF;
			contextLengthOut = 3;	// record the context length to be returned in the handshake
		}
		if (nContextLength > 3)
		{
			// set pContext[3] to the pContext[1] value at start of repeat series
			pContext[3] = xContext[1];
			contextLengthOut = 4;	// record the context length to be returned in the handshake
		}
		if (nContextLength > 5)
		{
			// set pContext[4,5] to the pContext[0,1] values at end of repeat series
			pContext[4] = yContext[0];
			pContext[5] = yContext[1];
			contextLengthOut = 6;
		}
		if (nContextLength > 7)
		{
			// Incoming, pContext[6,7] pass a 64-bit pointer to a Counts array used to calculate
			// the frequency of individual decodes.  Outgoing, pContext[6] provides the frequency of
			// the repeat series being reported in this return.
			pContext[6] = getFreq(pContext[2] & 0xFFFFF, (pContext[0] & 0xFFFFF) + (pContext[1]>>16) + MinFrame);
			contextLengthOut = 8;
		}
		if (nContextLength > 8)
		{
			// pContext[8] returns the note and aux note indexes (1-based, so add 1) for use by
			// IRScope, and the parameter value for the note.
			pContext[8] = iParam | (xNote_out + 1)<<16 | (xAuxNote_out + 1)<<24;
			contextLengthOut = 9;
		}

		if (contextLengthOut > 2)
		{
			// The handshake is that the 12-bit field in pContext[0] that normally carries the
			// preemptLength value is now used to carry a negative value, -(contextLengthOut-2).
			// The calling program checks to see if the top 8 bits of this value are 0xFF.  If
			// so then it is interpreted as a context length value, otherwise as a preemptLength
			// value.  So the maximum supported context length is 18, corresponding to 0xFF0.
			pContext[0] |= (2-contextLengthOut)<<20;
		}

		n = 0;
		if ( !bSuppressCount )
		{
			// nDittos = -1 means no dittos AND treat repeat frames as separate signals
			if ( xFrameCount == 1 && xDittos <= 0 )
			{
				n = sprintf(pMisc, *xMisc == 0 ? "no repeat" : "no repeat: ");
			}
			else if ( xFrameCount == 2 && xDittos <= 0 )	// nDittos < 0 should not occur in this case
			{
				n = sprintf(pMisc, *xMisc == 0 ? "+ 1 copy" : "+ 1 copy: ");
			}
			else if ( xDittos <= 0 )	// nDittos < 0 should not occur in this case
			{
				n = sprintf(pMisc, *xMisc == 0 ? "+ %d copies" : "+ %d copies: ", xFrameCount-1);
			}
			else if ( xFrameCount == 1 && xDittos == 1 )
			{
				n = sprintf(pMisc, *xMisc == 0 ? "+ 1 ditto" : "+ 1 ditto: ");
			}
			else if ( xFrameCount == 1 )
			{
				n = sprintf(pMisc, *xMisc == 0 ? "+ %d dittos" : "+ %d dittos: ", xDittos);
			}
			else	// this case should not occur
			{
				n = sprintf(pMisc, *xMisc == 0 ? "+ %d dittos, %d copies" : "+ %d dittos, %d copies: ", 
						xDittos, xFrameCount-1);
			}
		}
		// next line reports nStart and nFrameL for diagnostic purposes
		// n += sprintf(pMisc+n, "S=%d L=%d ", wContext[0]&0xFFFFF, (wContext[1]>>16)+MinFrame);
		strcpy(pMisc+n, xMisc);	// append original pMisc text
		*pError = 0;
	}
}

int Signal::msb(int val, int bits=8)	
{
	unsigned int t = ((val>>16)&0xFFFF) + ((val&0xFFFF)<<16);
	t = ((t>>8)&0xFF00FF) + ((t&0xFF00FF)<<8);
	t = ((t>>4)&0xF0F0F0F) + ((t&0xF0F0F0F)<<4);
	t = ((t>>2)&0x33333333) + ((t&0x33333333)<<2);
	t = ((t>>1)&0x55555555) + ((t&0x55555555)<<1);
	return t >> (32-bits);
}

int __cdecl compare( const void *arg1, const void *arg2 )
{
   return (int) ( * ( float* ) arg1 - * ( float* ) arg2 );
}

unsigned int Signal::getMsb(int first, int bits)
{
	int byte = first >> 3;
	int bt = 8-(first & 7);
	unsigned int rslt = cBits[byte] & ((1<<bt)-1);
	if ( bt >= bits )
	{
		return rslt >> (bt - bits);
	}
	for (bits-=bt; bits>=8; bits-=8)
		rslt = (rslt<<8) + cBits[++byte];
	return (rslt<<bits) + ((unsigned int)(cBits[1+byte]) >> (8-bits));
}

unsigned int Signal::getLsb(int first, int bits)
{
	int byte = first >> 3;
	int bt = first & 7;
	unsigned int rslt = cBits[byte] >> bt;
	bt = 8-bt;
	if ( bt >= bits )
	{
		return rslt & ((1<<bits)-1);
	}
	while ( bt+8 < bits )
	{
		rslt += cBits[++byte] << bt;
		bt += 8;
	}
	return rslt + ( ( cBits[1+byte] & ((1<<(bits-bt))-1) ) << bt );
}

void Signal::makeMsb()
{
	for (int ndx=(nBit+7)>>3; --ndx>=0; )
		cBits[ndx] = msb( cBits[ndx] );
}

void Signal::cleanup()
{
	memset(cBits, 0, sizeof(cBits));
	pBit = pFrame;
	nBit = 0;
	nState = 0;
}

void Signal::decodeX( int nCount )
{
	_ASSERT(nBit+nCount <= sizeof(cBits)*8);
	while (--nCount >= 0)
	{
		float nDelta = nMaxShort - *pBit;
		if ( nDelta < 0 )
		{
			cBits[nBit>>3] |= 1 << (nBit & 7);
		}
		pBit += 2;
		nBit++;
	}
}

void Signal::decodeX2( int nCount )
{
	_ASSERT(nBit+nCount <= sizeof(cBits)*8);
	while (--nCount >= 0)
	{
		if ( pBit[0]+pBit[1] > nMaxShort )
		{
			cBits[nBit>>3] |= 1 << (nBit & 7);
		}
		pBit += 2;
		nBit++;
	}
}

int Signal::checkDecodeX( int start, int count, float minShort, float maxLong, float maxFront )
{
	_ASSERT(nBit+count <= sizeof(cBits)*8);
	float *pB = pFrame+start;
	if (   pB >= pMainLimit
		|| pB+2*count > pFullLimit )
	{
		return 0;
	}
	while (--count >= 0)
	{
		double burst = pB[0]+pB[1];
		if (   burst < minShort
			|| burst > maxLong
			|| pB[0] > maxFront )
		{
			return 0;
		}
		if ( burst > nMaxShort )
		{
			cBits[nBit>>3] |= 1 << (nBit & 7);
		}
		pB += 2;
		nBit++;
	}
	return 1;
}

int Signal::decodeRaw( int bitsRequested )
{
	_ASSERT(nBit+bitsRequested < sizeof(cBits)*8);
	while ( bitsRequested > 0 )
	{
		if (pBit>pFrameEnd)
		{
			return 0;
		}
		double x =   *pBit * m_rawUnit
				   + ( ((pBit-pFrame)&1) ? m_rawGapAdjust : m_rawPulseAdjust );
		double y = floor(x);
		// GD Added test pBit<pFrameEnd as this return should not occur on a lead-out
		if ( pBit < pFrameEnd && x-y > m_rawErrorLimit || y==0. )
		{
			return 0;
		}
		int bits = (int)y;
		if ( (bitsRequested -= bits) < 0 )
		{
			bits += bitsRequested;
		}
		if ( (pBit-pFrame) & 1 )
		{
			nBit += bits;
		}
		else
		{
			do
			{
				cBits[nBit>>3] |= 1 << (nBit & 7);
				nBit++;
			} while (--bits);
		}
		pBit++;
	}
	return 1;
}

// The inner requirement is:
//    dBitMax >= value / ( bits + .3 )
//    dBitMin <= value / ( bits - .3 )
// Which implies:
//    bits >= value / dBitMax - .3
//    bits <= value / dBitMin + .3
//
// dNewMin = value / ( bits + .3 )
// dNewMax = value / ( bits - .3 )
// This implies an overall constraint of
//    dBitMin >= max_value / ( max_bits + .3 )
//    dBitMax <= min_value / .7
//
int Signal::decodeAsync(
                        float *pData,
                        int bits,   // previously decoded bits
                        int sizes,  // Bitmask of integerized durations seen
                        double dBitMin,
                        double dBitMax,
                        int bitsPerByte, // 10 or 11 = start + 8 data + 1 or 2 stop
                        int minTotBits )
{
	if (bits >= (int)sizeof(cBits)*bitsPerByte)
		return 0;
	int phase = bits % bitsPerByte;
	int pole = ( pData - pFrame ) & 1;  // 0==pulse  1==gap
	int minBits = (int)(*pData / dBitMax + .69999);  // fewest bits this value might be
	if ( minBits == 0 )
		minBits = 1;
	int maxB = (pole ? bitsPerByte : 9 ) - phase;    // most bits that would be valid here
	int maxBits = (int)(*pData / dBitMin + .30001);  // most bits this value might be
	if ( pData == pFrameEnd )
	{
		if (bits < minTotBits || maxBits <= maxB)
			return 0;
		sizes &= sizes - 1; // clear one bit
		if ( ( sizes & (sizes-1) ) == 0 )
			return 0;  // Need to see at least three sizes
		int bytes = bits/bitsPerByte + 1;
		memset(cBits,0xFF,bytes);
		nMinShort = floor(dBitMin);
		nMaxShort = ceil(dBitMax);
		return bytes;
	}
	if ( maxBits > maxB )
		maxBits = maxB;
	for (int nGuess = minBits; nGuess <= maxBits; ++nGuess)
	{
		if (pole && nGuess+phase > 8 && nGuess < maxB)
			continue;
		double dNewMin = *pData / (nGuess+.3);
		if (dNewMin < dBitMin)
			dNewMin = dBitMin;
		double dNewMax = *pData / (nGuess-.3);
		if (dNewMax > dBitMax)
			dNewMax = dBitMax;
		if ( dNewMin <= dNewMax )
		{
			int result = decodeAsync(pData+1, bits+nGuess, sizes | (1<<nGuess), dNewMin, dNewMax, bitsPerByte, minTotBits);
			if (result)
			{
				if (!pole)
					cBits[bits/bitsPerByte] &= (unsigned char)( ((((0xFF<<nGuess)+1)<<phase) - 1) >>1 ); 
				return result;
			}
		}
	}
	return 0;
}

unsigned int Signal::getFreq(int start, int end)
{
	int s=0;
	double t=0;
	double freq;

	if (pCounts == NULL) return 0;

	for (int n=start; n<end; n++)
	{	
		{
			s += pCounts[2*n];
			t += pDuration[2*n];
		}
	}
	freq = ((s!=end-start)&&(t > 0)) ? s*1000000./t : 0;

	return (unsigned int)freq;
}


void Signal::tryPid13()
// GD 2009 Added comment:  Decode assumes frame contains at least one 1 bit
{
	// {37.9k,1082}<0,-1|1,-0>(1,-1,F:6,-26)+
	if ( nFrameL > 5 )	
		return;
	if (   pFrame[0] < 900.
		|| pFrame[0] > 1200. )
	{
		return;
	}

	if ( ! framed(nTotDur) )
		return;
	m_rawUnit =  1. / pFrame[0];
	m_rawPulseAdjust = .4f;  // Pulse from -.4 to +.2
	m_rawGapAdjust = .2f;    // Gap from -.2 to +.4
	m_rawErrorLimit = .6f;

	cleanup();
	pBit += 1;
	if ( ! decodeRaw(7) || pBit < pFrameEnd )	
	{
		return;
	}

	// GD 2009 Start: Add any trailing 0's concatenated with lead-out into 
	// nTotDur and re-test framing.  Eliminates spurious decodes of final
	// 2 frames of G.I.4DTV and probably others.
	float n = 7.f;	// number of trailing 0's
	unsigned char xBits = cBits[0];
	for ( ; xBits != 0; n-- ) { xBits >>= 1; }
	if ( ! framed(nTotDur + (n - m_rawGapAdjust) / m_rawUnit ) )
	{
		return;
	}
	// GD 2009 End

	strcpy(pProtocol,"pid-0013");
	*pOBC = getLsb(1,6);
	*pHex = ((msb(cBits[0]) >> 1) & 0x3F) | 0x80;
}

// Lutron uses a 40kHz carrier to send an asynchronous signal with 8 start bits,
// 24 data bits and 4 stop bits.  The data bits are 6 4-bit values, each being
// the encoding in reversed binary with odd parity of a 3-bit binary value.  The 
// 18-bit sequence so encoded consists of an 8-bit device code, an 8-bit OBC and 
// two even parity bits calculated pairwise, all sent msb.
void Signal::tryLutron()
{
	if ( nFrameL < 4 || nFrameL > 10)	
		return;

	if (   pFrame[0] < 14000
		|| pFrame[0] > 30000
		|| nTotDur < 26*2300		// Nominal min is 29 units
		|| nTotDur > 35*2300 		// Nominal max is 32 units
		|| sortOn.min1 < 2100		// Nominal min is 2100
		|| sortOff.min1 < 2100 )	// Nominal min is 2100
	{
		return;
	}
		
	int bParityError = true;
	// Number of bits between lead-in and lead-out can be anything from 18 to 24
	for (int nBitCnt = 18; bParityError && nBitCnt < 25; nBitCnt++)
	{
		// Work out m_rawUnit from the total duration of these 18 to 24 bits
		m_rawUnit =  float(nBitCnt) / (nTotDur - pFrame[0]);
		m_rawPulseAdjust = .4f;				// Pulse from -.4 to +.2
		m_rawGapAdjust = .2f;				// Gap from -.2 to +.4
		m_rawErrorLimit = .6f;

		cleanup();
		pBit += 1;
		if ( ! decodeRaw(nBitCnt+4) || pBit < pFrameEnd ) continue;
		makeMsb();
		// Check that nBitCnt really is the number of bits before the lead-out.
		if ( ! (getMsb(nBitCnt-1, 1) == 1 && getMsb(nBitCnt, 8) == 0 ) ) continue;

		// Do not rely on the length of the lead-in distinguishing correctly between it being
		// 8,9,10 or 11 units, i.e. concatenated with 0 to 3 data 1-bits.  So loop through 
		// the possibilities until we get a valid decode.
		for (int nOnes=0; bParityError && nOnes<25-nBitCnt; nOnes++)
		{
			// Store the 18-bit decode of the 24-bit data starting at cBits[4]
			cBits[4] = cBits[5] = cBits[6] = 0;
			int nBitPos = 0;	// Next position within 18-bit decode
			bParityError = false;
			for (int i=0; i<24; i++)
			{
				// To decode 3-bit reversed binary with odd parity into 3-bit binary, each binary
				// bit starting msb is XOR of all the reversed binary bits from msb to the corresponding
				// bit, e.g. 1101 decodes to 100 since 1=1, 0=1^1, 0=1^1^0.  The parity check requires
				// XOR of all four bits to be 1, so finally test 1^1^0^1 == 1.
				if ( i<nOnes ? 1 : getMsb(i-nOnes, 1) )
				{
					for (int n = nBitPos; n < (i/4)*3 + 4; n++) { cBits[4+(n>>3)] ^= 0x80>>(n&7); }
				}
				if ((i&3)==3)
				{
					bParityError |= (getMsb(nBitPos+32,1) != 1);
					// Clear the bit position where the parity check was temporarily stored.
					cBits[4+(nBitPos>>3)] &= (0xFF - (0x80>>(nBitPos&7)));
				}
				else nBitPos++;
			}

			// Now check the two overall parity bits of the 18-bit decode
			int nOverallParity = 0;
			for (int i=0; i < 9; i++) nOverallParity ^= getMsb(32+2*i, 2);
			bParityError |= (nOverallParity != 0);
			// If all OK, check that number of initial 1-bits is consistent with the lead-in duration.
			// Note that overall parity means that if nOnes is in error then it can only be out by 2.
			if (	pFrame[0]*m_rawUnit < 7.2 + nOnes
				||	pFrame[0]*m_rawUnit > 8.8 + nOnes ) continue;
/*
			char str[100];
			sprintf(str, "Lutron bitcnt=%d nOnes=%d data = %X", nBitCnt, nOnes, getMsb(32,18));
			MessageBox(0,str,"DecodeIR",0);
*/
		}
	}
	if (bParityError) return;
	strcpy(pProtocol,"Lutron");
	*pDevice = getMsb(32,8);
	*pOBC = getMsb(40,8);		// Hex is 2-byte so cannot be shown
}

void Signal::tryPCTV()
{
	// {38.4k,832}<0,-1|1,-0>(2,-8,1,D:8,F:8,2,-???)

	if ( nFrameL > 10 )
		return;

	// nTotDur is nominal 29
	if (   nTotDur < 27.*832.
		|| nTotDur > 31.*832. )
	{
		return;
	}
	if ( pFrame[0] > (2.3/29.) * nTotDur )
		return;
	double nominal10 = pFrame[0]+pFrame[1];
	if (   nTotDur > 3.0 * nominal10      
		|| nTotDur < 2.8 * nominal10 )
	{
		return;
	}
	m_rawUnit =  29. / nTotDur;
	m_rawPulseAdjust = .5;  // Pulse from -.5 to +.25
	m_rawGapAdjust = .25;   // Gap from -.25 to +.5
	m_rawErrorLimit = .75;

	cleanup();
	pBit += 2;
	if ( ! decodeRaw(19) || pBit != pFrameEnd )
	{
		return;
	}
	if ( cBits[2]<6 )
		return;
	strcpy(pProtocol,"PCTV");
	*pDevice = getLsb(1,8);
	*pOBC = getLsb(9,8);
	sprintf(pMisc,"h=%02X %02X", msb(*pDevice), msb(*pOBC));
}

void Signal::tryGap()
{
	_ASSERT(nFrameL <= sizeof(cBits)*8+1);

	if ( nFrameL < 6 )
		return;

	if ( sortOn.max1*2. > sortBurst.min1+sortOn.min1
		// Need additional condition to cope with RCA(Old)
		&& ( nFrameL != 25 || sortOn.max1 != pFrame[48] || sortOn.max2*2. > sortBurst.min1+sortOn.min1) 
		// Allow longer On if may be part of NEC leadout-- for Onkyo signals with leadout On = 32/21*(ordinary On)
		&& ( nFrameL != 33 || pFrame[65] < 30.*sortOn.min1 || sortOn.max1*1.5 > sortBurst.min1+sortOn.min1) )  //DAR Dec 2010 
		return;

	float nFrameLimit = sortBurst.max1*2-sortBurst.min1;
	float nFrame = *pFrameEnd;
	if (nFrame <= nFrameLimit)
		return;						// Not well enough framed


	nMaxShort = (sortBurst.mid1 + sortBurst.mid2) * .5;
	switch (nFrameL - 1)    // Special cases while checking for two burst sizes
	{
	case 13:
		//  (0k,16.7)<1,-5|1,-11>(1,-17, D:5,F:6, 1,-17,1,-7000)+
		//  (0k,10  )<1,-5|1,-15>(1,-25, D:5,F:6, 1,-25,1,120m)+
		if ( nMaxShort > sortBurst.max3 )
		{
			if (   nMaxShort < pFrame[1]  
				&& nMaxShort < pFrameEnd[-2] )
			{
				nMaxShort = sortBurst.max1*.4;
				break;
			}
			goto find_two_sizes;
		}
		else
		{
			float falseDelta  = sortBurst.mid1-sortBurst.min1;
			float falseDelta2 = sortBurst.max3-sortBurst.mid2;
			if ( falseDelta < falseDelta2 )
				falseDelta = falseDelta2;
			if ( sortBurst.mid2*.9-sortBurst.mid1 < falseDelta )
			{
				// Can't see a valid gap between two burst sizes
				return;
			}
		}
		break;

	default:
find_two_sizes:
		{
			float falseDelta  = sortBurst.mid1-sortBurst.min1;
			float falseDelta2 = sortBurst.max1-sortBurst.mid2;
			if ( falseDelta < falseDelta2 )
				falseDelta = falseDelta2;
			if ( sortBurst.mid2*.95-sortBurst.mid1 < falseDelta )
			{
				// Can't see a valid gap between two burst sizes
				if ( sortBurst.min1*1.3 < sortBurst.max1 )
					return;  // Sizes vary enough that there should have been a gap
				nMaxShort = 0;  // We have only one size
			}
		}
	}

	float nFrame0=-1.; 			// Frame gap assuming no lead in
	nFrame1 = nFrame;			// Frame gap assuming lead in
	nFrame2 = nFrame;			// Frame gap assuming alternate lead in

	pLead = NULL;					// Possible location of Lead-in
	if ( pFrame > pDuration )
	{
		pLead = pFrame - 2;
		nFrame0 = pLead[1];
		if ( pLead[0]+pLead[1] <= sortBurst.max1 )   // ??? Tighten
			goto no_lead1;
		if ( pLead>pDuration )
		{
			float leadFrame = pLead[-1];
			if (pLead == pDuration + 2*nSingle)
			{
				float loopFrame = pLead[2*nRepeat - 1];
				if ( loopFrame > leadFrame )
					leadFrame = loopFrame;
			}
			if ( nFrame1 > leadFrame )
				nFrame1 = leadFrame;
		}
		if ( nFrame1 <= nFrameLimit )
		{
			goto no_lead1;
		}
		if (pLead[1]*(.9) >= nFrame1)
		{
		no_lead1:
			pLead = NULL;
		}
	}

	pLead2 = NULL;					// Alternate possible location of Lead-in
	if ( pFrame == pDuration + 2*nSingle )  // If frame is beginning of repeat part
	{
		pLead2 = pFrame-2 + 2*nRepeat;      // Lead in might be end of repeat part
		if ( nFrame0 < pLead2[1] )
			nFrame0 = pLead2[1];
		if ( pLead2[0]+pLead2[1] <= sortBurst.max1 )   // ??? Tighten
			goto no_lead2;
		if (pLead2[-1] < nFrame2)
		{
			nFrame2 = pLead2[-1];
			if (nFrame2 <= nFrameLimit)
				goto no_lead2;
		}
		if (pLead2[1]*(.9) >= nFrame2)
			goto no_lead2;
		if (pLead == NULL)
		{
			pLead = pLead2;
			nFrame1 = nFrame2;
		no_lead2:
			pLead2 = NULL;
		}
	}
	if ( nFrame0 < 0 || nFrame0 > nFrame )
	{
		nFrame0 = nFrame;
	}
	if (pLead==NULL && nFrame0 <= nFrameLimit)
	{
		return;
	}

	int nSpecial = 0;
	enum
	{
		Special_Proton = 1,
		ViewStar_prequalify = 2,
        Special_Konka = 4,
        Special_Feelux = 8,
        Special_Samsung36 = 16,
	};

	if (nMaxShort == 0)
	{
		// Try for all bits equal
		switch (nFrameL - 1)
		{
		case 5:
			{
				// pid-0001 {0k,msb}<24,-9314|24,-13486>(24,-21148,(F:5,1,-28m)+)
				// Jerrold {0k,44}<1,-7.5m|1,-11.5m>(F:5,1,-23.5m)+
				if (   sortOn.max1 < 180 
					&& sortBurst.min1 > 7000
					&& sortBurst.max1 < 15000
					&& nFrame0 > 18000
					&& (   unmodulated(sortOn.max1)
						|| sortOn.max1*nFreq > 50000000) )// Kludge for strange example of Jerrold seen in CML file )
				{
					nMaxShort = 10000.;
					break;
				}
				// Viewstar {50.5k,337}<1,-8|1,-5>(F:5,1,-17)+
				if (      sortOn.max1 < (1.15*337)
					&&   nFrame0 > 4000.
					&&   nFrame0 > sortBurst.max1*1.5
					&&   nFrame0 > sortOn.max1*14
					&& (   sortBurst.max1 < (1.2*9*337) && sortBurst.min1 > 7.5*337		// GD 2009: max changed
						|| sortBurst.max1 < 7.5*337 && sortBurst.min1 > (.8*6*337) ) )	// GD 2009: min changed
					/*
					&& (   sortBurst.max1 < (1.2*6*337) && sortBurst.min1 > 7.5*337		// GD 2009: these original inequalities cannot
						|| sortBurst.max1 < 7.5*337 && sortBurst.min1 > (.8*9*337) ) )	// be correct as first line is inconsistent
					*/
				{
					nSpecial = ViewStar_prequalify;
					nMaxShort = 7.5*337;
					break;
				}
			}
			return;
        case 6:
            // pid-0004  {0k,msb}<12,-130|12,-372>(F:6,12,-27k)+
            if (   sortOn.max1 < 60
                && sortBurst.min1 > 100
                && sortBurst.max1 < 500
                && nFrame0 > 20000 )
            {
                nMaxShort = 260;
                break;
            }
            return;
		case 16:
			// G.I. cable:  device 0, function 0 {38.7k,490}<1,-4.5|1,-9>(18,-9,F:8,D:4,C:4,1,-84,(18,-4.5,1,-178)*)
			// All Bursts = nominal 5.5
			// MaxShort should be 7.75
			nMaxShort = sortBurst.max1*( 7.75/5.5 );
			if ( lead_in( .7*27/7.75*nMaxShort, 1.3*27/7.75*nMaxShort, 1.3*18/7.75*nMaxShort, .7*9/7.75*nMaxShort, nMaxShort ) )
			{
				break;
			}
			return;
		default:
			return;
		}
	}

	// Protocol specific preprocessing
	switch (nFrameL - 1)
	{
	case 12:
		{
			// Try to distinguish misframed Nokia from properly framed G.I. etc.
			if (   nFrame0 <= nFrameLimit
				&& pLead[0] < sortOff.max1
				&& (pLead2==0 || pLead2[0] < sortOff.max1 ) )
			{
				return;
			}
		}
	case 17:
		{
			// Proton {38k,500}<1,-1|1,-3>(16,-8,D:8,1,-8,F:8,1,^63m)+
			//
			// sortBurst.max1 = nominal 9
			if ( lead_in( .7*8/9*sortBurst.max1, 1.3*24/9*sortBurst.max1, 1.3*16/9*sortBurst.max1, .7*8/9*sortBurst.max1, sortBurst.max1 )
				&& sortOff.max1 == pFrame[17]
				&& sortBurst.max2*1.6 < pFrame[17] )
			{
				nMaxShort = sortBurst.max1*.33;
				nSpecial = Special_Proton;
			}

            if ( sortBurst.min1*2.8 < sortBurst.max1 )
                // Konka rejected:  nominal (4 or 6)*2.8 > nominal 9
            {
                // Feelux {38k,290,msb}<2,-2|2,-4>(2,-16,F:8,~F:8,2,-11,2,-53)+
			    //
			    // sortBurst.max1 = nominal 15
			    // sortBurst.max2 = nominal 6
                if (   lead_in( .7*17/13*sortBurst.max1,  // The initial 2,-16 must be >= 70% of a nominal 17
                              1.3*18/13*sortBurst.max1,   // The initial 2,-16 must be <= 130% of a nominal 18
                              1.3*2/13*sortBurst.max1,    // The initial 2 must be <= 130% of a nominal 2
                              .7*16/13*sortBurst.max1,    // The initial -16 must be >= 70% of a nominal 16
                              sortBurst.max1  )           // The framing gap must be a least a nominal 13 greater than the leadin total
                    &&  sortOff.max1 == pFrame[33]        // The largest Off must be the one in the first lead out burst
                    &&  sortBurst.max2*.7*11/6 < pFrame[33] ) // Second largest burst check
                {
				    nMaxShort = (5./13)*sortBurst.max1;
				    nSpecial = Special_Feelux;
                }
                break;
            }
            // Konka {38k,500,msb}<1,-3|1,-5>(6,-6,D:8,F:8,1,-8,1,-46)+
			//
			// sortBurst.max1 = nominal 9
			// sortBurst.max2 = nominal 6 or rarely 4
            if (   lead_in( .7*7/9*sortBurst.max1,   // The initial 6,-6 must be >= 70% of a nominal 7
                          1.3*12/9*sortBurst.max1,   // The initial 6,-6 must be <= 130% of a nominal 12
                          1.3*6/9*sortBurst.max1,    // The initial 6 must be <= 130% of a nominal 6
                          .7*6/9*sortBurst.max1,     // The initial -6 must be >= 70% of a nominal 6
                          sortBurst.max1  )          // The framing gap must be a least a nominal 9 greater than the leadin total
                &&  sortOff.max1 == pFrame[33]       // The largest Off must be the one in the first lead out burst
                &&  sortBurst.max2*.7*8/6 < pFrame[33] ) // Second largest burst check
            {
				nMaxShort = (5./9)*sortBurst.max1;
				nSpecial = Special_Konka;
				break;
            }
		}
    case 37:
        {
            // Samsung36 {38k,500}<1,-1|1,-3>(9,-9,D:8,S:8,1,-9,E:4,F:8,-68u,~F:8,1,-118)+
            //
			// sortBurst.max1 = nominal 10
			// sortBurst.max2 = nominal 4
            // 
            if (   lead_in( .7*10/10*sortBurst.max1, // The initial 9,-9 must be >= 70% of a nominal 10
                          1.3*18/10*sortBurst.max1,  // The initial 9,-9 must be <= 130% of a nominal 18
                          1.3*9/10*sortBurst.max1,   // The initial 9 must be <= 130% of a nominal 9
                          .7*9/10*sortBurst.max1,    // The initial -6 must be >= 70% of a nominal 6
                          sortBurst.max1  )          // The framing gap must be a least a nominal 10 greater than the leadin total
                &&  sortOff.max1 == pFrame[33]       // The largest Off must be the one in the first lead out burst
                &&  sortBurst.max2*.7*8/6 < pFrame[33] ) // Second largest burst check
            {
				nMaxShort = (3./10)*sortBurst.max1;
				nSpecial = Special_Samsung36;
            }
        }
	}


	int bLeadIn = 0;
	//if ( nMaxShort > pOff[nFrameL-3] && pFrame[1]==pOff[nFrameL-2] && nFrame0>nFrameLimit)
	//{
		// Sure looks like lead in, but how to be sure?

		/*
		// Blaupunkt{prefix}: {500}<-1,1|1,-1>(1,-5,1023:10,-27)
		if (   nFrameL == 11
			&& sortOn.max1 > 400 && sortOn.max1 < 600
			&& pOff[nFrameL-3] > 400 && pOff[nFrameL-3] < 600
			&& pOff[nFrameL-2] < 3000 )
		{
			strcpy(pProtocol, "Blaupunkt{prefix}");
			return;
		}
		*/
		//bLeadIn = 1;
	//}
	cleanup();
	decodeX2( nFrameL - 1 );

	switch (nBit)
	{
	case 5:
		{
			// pid-0001 {0k,msb}<24,-9314|24,-13486>(24,-21148,(F:5,1,-28m)+)
			if (   sortOn.max1 < 1000
				&& sortBurst.min1 > 8300
				&& sortBurst.max1 < 15000
				&& ( sortBurst.mid1 < 10300 || sortBurst.min1 > 12400 )
				&& ( sortBurst.mid2 > 12400 || sortBurst.max1 < 10300 )
				&& pLead
				&& pLead[0] < 1000
				&& pLead[1] > 19000
				&& pLead[1] < 23000
				&& (   unmodulated(sortOn.max1)
					|| sortOn.max1*nFreq > 50000000) )
			{
				strcpy(pProtocol, "pid-0001");
				*pOBC= msb(cBits[0],5);
				*pHex = (31-*pOBC)<<3;
				if ( checkDecodeX( 12, 5, 8300., 15000., 1000.) )
				{
					if ( getLsb(0,5) == getLsb(5,5) )
					{
						newPreemptValue = prByPid_0001;    // Avoid Jerrold decode of repeat part
						newPreemptLength = 7;
					}
				}
				return;
			}
			// Jerrold {0k,44}<1,-7.5m|1,-11.5m>(F:5,1,-23.5m)+
			// 0006:
			// Archer {0k,12}<1,-3.3m|1,-4.7m>(F:5,1,-9.7m)+
			//
			if (   sortOn.max1*40 < sortBurst.min1
				&& sortBurst.min1*2 > sortBurst.max1
				&& nFrame0 > 1.2*sortBurst.max1
				&& (   unmodulated(sortOn.max1)
					|| sortOn.max1*nFreq > 50000000) )// Kludge for strange example of Jerrold seen in CML file )
			{
				if ( preemptValue >= prJerrold )
					return;
				strcpy(pProtocol, (sortOff.max1>6000) ? "Jerrold" : "Archer");
				*pOBC= cBits[0];
				*pHex = msb(cBits[0]);
				return;
			}
			// Viewstar {50.5k,337}<1,-8|1,-5>(F:5,1,-17)+
			// 0021:
			//
			//
			if (nSpecial == ViewStar_prequalify
				||
						   sortOn.max1 < (1.15*337)
					&&     sortOn.max1 < (1.2/6)*sortBurst.min1
					&&  sortBurst.min1 > (.8*6*337)
					&& sortBurst.mid1 < (1.2*6*337)
					&& sortBurst.mid2 > (.8*9*337)
					&& sortBurst.max1  < (1.2*9*337)
					// GD 2009 Next line, nominal value is 1.5, so 1.48 must be wrong
					&& sortBurst.max1  < /* 1.48 */ 1.2 * 1.5 * sortBurst.min1 
					&&    nFrame0 > 4000. )
			{
				strcpy(pProtocol, "Viewstar");
				*pOBC= cBits[0];
				*pHex = msb(31-cBits[0]);
				return;
			}
			// ?? {57.6k,360}<1,-12|1,-8>(F:5,1,-25)+
			// 00E9:
			//
		}
		return;  // Too small for Gap decode
    case 6:
        {
            // pid-0004  {0k,msb}<12,-130|12,-372>(F:6,12,-27k)+
            if (   sortOn.max1 < 60
                && sortBurst.min1 > 100
                && sortBurst.max1 < 500
                && nFrame0 > 20000 )
            {
				    strcpy(pProtocol, "pid-0004");
				    *pOBC= msb(cBits[0],6);
    				*pHex = 255-msb(cBits[0]);
                    return;
            }
        }
		return;  // Too small for Gap decode
	case 7:
		return;  // Too small for Gap decode
	case 8:
		{
			// Proton {38.9k,530}<1,-1|1,-3>(16,-8,D:8,1,-8,F:8,1,-40)+
			// 005C:
			// Audiovox {40k,500}<1,-1|1,-3>(16,-8,D:8,1,-8,F:8,1,-40)+
			// 005D
		}
		break;
	case 10:
		{
			// pid-0083 {42.3K, 3000}<1,-3,1,-7|1,-7,1,-3>(F:5,1,-27)+
			// 0083: (used only by setup code TV/0159, Fisher, Sanyo, Sears)
			//
			// ???? {35k,300}<1,-3|1,-7>(D:3,F:7,1,^29m)+
			//
			// Akai {38k,289}<1,-2.6|1,-6.3>(D:3,F:7,1,^25.3m)+
			// 000D:
			//
			// ???? {40k,800}<1,-3|1,-7>(F:5,~F:5,1,-60)+
			//
			if (   sortOn.max1*3.2 < sortBurst.min1
				&& sortBurst.mid1*1.6 < sortBurst.mid2
				&& sortBurst.min1*2.4 > sortBurst.max1 )
			{
				if (   sortBurst.max1 > 24000 && sortBurst.min1 < 24000
					&& nFrame0 > 2*sortBurst.max1 )
				{
					int t=getLsb(0,10);
					int a=0;
					for (int n=5; ;)
					{
						a <<= 1;
						int b=t&3;
						t >>= 2;
						if (b==1)
							a++;
						else if (b!=2)
							break;
						if (--n==0)
						{
							strcpy(pProtocol, "pid-0083" );
							makeMsb();
							*pOBC = a;
							*pHex = (31-*pOBC)*8;
							return;
						}
					}
				}
				if (   nFrame0 > 3*sortBurst.max1
					&& nMaxShort < 2000. )
				{
					strcpy(pProtocol, "Akai" );
					*pDevice = getLsb(0,3);
					*pOBC = getLsb(3,7);
					makeMsb();
					*pHex = 255-getMsb(2,8);
					return;
				}
			}

			// Matsui {38K,525}<1,-1|1,-3>(D:3,F:7,1,^30.5m)+
			if (   nFrame0 > 5*sortBurst.max1
				&& sortBurst.mid1*1.6 < sortBurst.mid2
				&& sortBurst.min1*2.4 > sortBurst.max1
				&& sortBurst.mid1 < 1.3*2*525
				&& sortBurst.mid2 > .7*4*525 )
			{
				strcpy(pProtocol, "Matsui" );
				sprintf(pMisc, "%.0f/%.0f-%.0f/%.0f-%.0f", sortOn.max1, sortBurst.min1, sortBurst.mid1, sortBurst.mid2, sortBurst.max1);
				*pDevice = getLsb(0,3);
				*pOBC = getLsb(3,7);
				return;
			}

			// ???? {40k,800}<1,-3|1,-7>(F:5,~F:5,1,-60)+
			if (   nFrame0 > 2.* sortBurst.max1
				&& sortBurst.mid1*1.6 < sortBurst.mid2
				&& sortBurst.min1*2.4 > sortBurst.max1 )
			{
				int f = getLsb(0,5);
				if ( (getLsb(5,5)^31) == f )
				{
					*pOBC = f;
					strcpy(pProtocol, "?10" );
					return;
				}
			}

		}
		break;
	case 11:
		{
			// RECS80 {38k,158,msb}<1,-31|1,-47>(1:1,T:1,D:3,F:6,1,-45m)+
			// 0045
			//        {33.3k,msb)<180,-5500|180,-8400>(1:1,T:1,D:3,F:6,1,^138m)+
			// 0068
			// Velleman {38k,msb}<700,-5060|700,-7590>(1:1,T:1,D:3,F:6,1,-55m)+
			if (   nFrame0 > sortBurst.max1*2.6
				// && (cBits[0]&1)
				//&& sortOn.max1*8 < sortBurst.min1  DAR Feb 2011
				&& sortBurst.min1 > .7*2/3 * sortBurst.max1 
				&& sortBurst.max1 < 18000 )	// Prevent spurious decode of 2 frames of Jerrold
			{	
				if (sortOn.max1*12 < sortBurst.min1)  //change 8 to 12 DAR Feb 2011
				{
					strcpy(pProtocol,"RECS80");
					makeMsb();
					*pDevice = getMsb(2,3);
					*pOBC = getMsb(5,6);
					*pHex = (63-*pOBC)<<2;
					sprintf(pMisc,"T=%d (%.0f/%.0f/%.0f)", getMsb(0,2)^2, sortOn.max1,sortBurst.mid1,sortBurst.mid2);
					return;
				}
				if (sortOn.max1*5 < sortBurst.min1 && sortBurst.min3 > 5000.)
				{
					strcpy(pProtocol,"Velleman");   //add DAR Feb 2011
					makeMsb();
					*pDevice = getMsb(2,3);
					*pOBC = getMsb(5,6);
					*pHex = (63-*pOBC)<<2;
					sprintf(pMisc,"T=%d", getMsb(1,1));
					return;
				}
			}
		}
		break;
	case 12:
		{
			// Pace MSS:      {38k,630,msb}<1,-7|1,-11>(1,-5,1,-5,T:1,D:1,F:8,1,^120m)+
			//
			if (   nFrame0 > 4*sortBurst.max1
				&& sortOn.max1*4 < sortBurst.min1
				&& pFrame[2]+pFrame[3] < sortBurst.min3*(6./7.)
				&& pFrame[0]+pFrame[1] < sortBurst.min3*(6./7.) )
			{
				if ( 10./6 * sortBurst.min1 > sortBurst.max1 )	// All bits were off
				{
					cBits[0] = cBits[1] = 0;
				}
				makeMsb();
				strcpy(pProtocol, "Pace MSS");
				*pDevice = getMsb(3,1);
				*pOBC = getMsb(4,8);
				sprintf(pMisc, "T=%d", getMsb(2,1));
				return;
			}
			// Thomson: OLD                 (D:4,T:1,D:1:5,F:6   OLD
			// Thomson: {33k,500}<1,-4|1,-9>(D:4,T:1,F:7,1,^80m)+  NEW  DAR Dec 2010 
					//http://www.hifi-remote.com/forums/viewtopic.php?t=12522
			// 004B
			// max_data = 12*500*(1+9) = 60m
			// min_gap = 80m-60m = 20m
			// min_ratio = 20 / (9*.5) = 4.44
			//
			if (   nFrame0    > 3.5*sortBurst.max1
				&& sortOn.max1*3.5 < sortBurst.min1
				&& sortBurst.mid1 < 1.3*.5 * sortBurst.mid2
				&& sortBurst.min1  > 1500. 
				&& sortBurst.min1  < 2700.)  //DAR to reject Lumagen
			{
				strcpy(pProtocol, "Thomson7");
				*pDevice = getLsb(0,4);
				*pOBC = getLsb(5,7);
				sprintf(pMisc, "T=%d", getLsb(4,1));
				sprintf(pMisc+19," %.0f-%.0f %.0f-%.0f %.0f-%.0f %.0f",sortOn.min1,sortOn.max1,sortBurst.min1,sortBurst.mid1,sortBurst.mid2,sortBurst.max1,nFrame0);
				pHex[0] = msb(127-*pOBC);
				return;
			}

			// G.I.4DTV  {37.3k,992}<1,-1|1,-3>(5,-2,F:6,D:2,C:4,1,-60)+
			// 00A4:
			// nMaxShort = nominal 3
			if (   lead_in( .8*7/3*nMaxShort, 1.2*7/3*nMaxShort, 1.2*5/3*nMaxShort, .8*2/3*nMaxShort, nMaxShort )
				&& sortBurst.mid1*1.5 < sortBurst.mid2 )
			{
				strcpy(pProtocol, "G.I.4DTV");
				*pOBC = getLsb(0,6);
				*pDevice = getLsb(6,2);
				int check = cBits[1];
				makeMsb();
				int W = getMsb(0,12);
				int syn =    parity(W & 0x9A8) << 3; //Hamming code for the 12 bits of F,D,and C
				syn = syn + (parity(W & 0xD74) << 2); //syn is the Hamming syndrome
				syn = syn + (parity(W & 0x6B2) << 1); 
				syn = syn + (parity(W & 0x351)     );
				//int nC = cBits[0];  removed June 2012 DAR
				//nC = nC ^ (nC<<4) ^ (nC<<1) ^ (nC>>2) ^ (nC>>4) ^ (nC>>5) ^ (nC>>6);
				//int nCC = cBits[1]*17; 
				if (syn == 0x00)
					return;
				else if (syn == 0x0F) {
					*pDevice += 4;
					return;
				}
				//if ( (nC^nCC)&30 ) removed June 2012 DAR
				//	sprintf(pMisc, "C=%d expected %d", cBits[1], (nC>>4)&1 | (nC&14) ); removed June 2012 DAR
				sprintf(pMisc, "synd. not recognized C=%d", check);
				return;
			}

			// Lumagen {38.4k,416,msb}<1,-6|1,-12>(D:4,C:1,F:7,1,-26)+  {C = odd parity for F}
			if (   nFrame0 > sortBurst.max1*1.5
				&& sortBurst.min1 > .7*7/13*sortBurst.max1
				&& sortBurst.mid1 < 1.3*7/13*sortBurst.mid2
				&& sortOn.max1 < sortBurst.max1*.1
				&& sortBurst.min1 > 2000. )
			{
				if ( parity( getLsb(4,8) ) )
				{
					makeMsb();
					strcpy(pProtocol, "Lumagen");
					pHex[0] = getMsb(4,8);
					*pOBC = pHex[0] & 127;
					*pDevice = getMsb(0,4);
					return;
				}
			}
		}
		break;
	case 13:
		{
			// Revox  {0k,15u}<1,-9|1,-19>(1:1,-10,0:1,D:4,F:6,1:1,-10,1:1,-100m)+  //added May 2012 DAR
			//        (0k,16.7)<1,-5|1,-11>(1,-17, D:5,F:6, 1,-17,1,120m)+
			// Barco  (0k,10  )<1,-5|1,-15>(1,-25, D:5,F:6, 1,-25,1,120m)+
			if (   nFrame0 > sortBurst.max1*8
				&& sortBurst.max3+sortOn.max1 < pFrame[1]
				&& sortBurst.max3+sortOn.max1 < pFrameEnd[-2] )
			{
				if (sortBurst.max2 > 350) {
					strcpy(pProtocol, "Revox" );
					*pDevice = getLsb(2,4);
					*pOBC = getLsb(6,6);
					return;
				} else
				{
					strcpy(pProtocol, "Barco" );
					*pDevice = getLsb(1,5);
					*pOBC = getLsb(6,6);
					*pHex = msb(63-*pOBC);
					sprintf(pMisc, "%.0f/%.0f/%.0f/%.0f", sortOn.max1, sortBurst.min1, sortBurst.max3, sortBurst.max2);
					return;
				}
			}
			// Dishplayer {38.4k,535,msb}<1,-5|1,-3>(1,-11,(F:6,U:5,D:2,1,-11)+)
			// 010F:
			if (   nFrame0 > nFrameLimit
				&& sortOn.max1*3 < sortBurst.min1
				&& sortBurst.min1 > .75*535*4
				&& sortBurst.min1 < 1.3*535*4
				&& sortBurst.min1 > .78*4/6*sortBurst.max1 )
			{
				// Set pLead to point to the initial (1, -11) lead-in so that
				// start is reported correctly by decode2()
				if (!nFrameCount && pFrame >= pDuration + 2) pLead = pFrame - 2;
				nNote_out = 2;
				makeMsb();
				strcpy(pProtocol, "Dishplayer" );
				*pDevice = 3-getMsb(11,2);
				*pSubDevice = 31-getMsb(6,5);
				*pOBC = 63-getMsb(0,6);
				return;
			}
		}
		break;
	case 15:
		{
			// Denon {38k,264}<1,-3|1,-7>(D:5,F:8,0:2,1,-165,D:5,~F:8,3:2,1,-165)+
			// Since the sequence of half frames can end in either a first or second half,
			// a better representation is:
			// Denon {38k,264}<1,-3|1,-7>(T=0,(D:5,<F:8,0:2|~F:8,3:2>(T:1),1,-165,T=T+1)2+)
			// 001C
			// 009C
			// Sharp {38k,264}<1,-3|1,-7>(D:5,F:8,1:2,1,-165,D:5,~F:8,2:2,1,-165)+
			// 001C:
			if (   nFrame0    > sortBurst.max1*4             // Constraints are loose before checking for both halves
				&& sortOn.max1*1.9 < sortBurst.min1
				&& sortBurst.min1  > .7*4/8 * sortBurst.max1
				&& sortBurst.mid1 < 1.3*4/8 * sortBurst.mid2 )
			{
				// nSecond & 0x40 determines if it is a second-half frame as in protocol spec.
				// nSecond & 0x01 determines if both half-protocol frames are present, in either order,
				//   in a valid arrangement (a first-half can be preceded or followed by a second-half
				//   but a second-half must be preceded by a first-half).

				int nSecond = cBits[1] & 0x40;  // 0x40 if second frame // 0 if first // later 1 if both
				
				// Check if other half-protocol frame follows it
				if (	pFrameEnd+32 < pDuration+2*nSingle+4*nRepeat
					&&	pFrameEnd[32] > sortBurst.max1*4 )
				{
					nBit = 16;
					pBit = pFrameEnd+1;
					decodeX2( 15 );				// Read following frame into cBits[2], cBits[3]
					if ( (cBits[0]^0xE0)==cBits[2] && (cBits[1]^0x7F)==cBits[3]  )
					{
						if (! nSecond )
						{							
							// First half-protocol frame followed by second half, so valid
							nSecond++;			// Flag it as valid
						}
					}
				}

				// If not yet confirmed as valid, check if other half-protocol frame precedes it
				pBit = pFrame - 32;				// Location of previous frame if there was one ?? use preempt instead
				if ( !( nSecond & 1 )			// If half-protocol not yet confirmed valid 
					&& ( pBit >= pDuration ))	// If that location is valid
				{
					nBit = 16;
					cBits[2] = cBits[3] = 0;	// Reset these from possible previous test
					decodeX2( 15 );				// Read previous frame into cBits[2], cBits[3]
					if ( (cBits[0]^0xE0)==cBits[2] && (cBits[1]^0x7F)==cBits[3] )
					{
						// Either half preceded by other half, so valid
						nSecond++;
					}
				}

				if (   (nSecond & 1)
					||   sortOn.max1*2.2 < sortBurst.min1   // Tighten constraints if only one half found
					  && sortBurst.mid1 < 1400. )
				{
					if (nSecond & 0x40)
					{
						cBits[0] ^= 0xE0;
						cBits[1] ^= 0x7F;
					}
					strcpy(pProtocol, (cBits[1]&0x20) ? "Sharp" : "Denon" );
					nNote_out = 7;
					// If half-protocol frame not in valid sequence, mark it in protocol name
					// and prevent it being aggregated in multiple frame count
					if ( ! (nSecond & 1) /*|| (nSecond & 0x40)*/ )
					{
						strcat( pProtocol, (nSecond & 0x40) ? "{2}" : "{1}" );
						nNote_out = 0;
						nAuxNote_out = (nSecond & 0x40) ? 4 : 3;
						bInitLeadIn = true;		// Here, this just prevents aggegation
					}
					*pDevice = getLsb(0,5);
					*pOBC = getLsb(5,8);
					*pHex = msb(*pOBC);
					newPreemptValue = prByDenon; 	// Avoid Gap decode of partial frame
					newPreemptLength = nFrameL;
					return;
				}
			}
		}
		break;
	case 16:
		{
			// Bose  {500,msb}<1,-1|1,-3>(2,-3,F:8,~F:8,1,-???)+
			//
			// nMaxShort = nominal 3
			if (   cBits[0] == 255-cBits[1]
				&& lead_in( nMaxShort, 2.*nMaxShort, nMaxShort, .8*nMaxShort, nMaxShort ) )
			{
				strcpy(pProtocol, "Bose");
				pHex[0] = *pOBC = msb(cBits[0]);
				return;
			}

			// G.I. Cable  {38.7k,490}<1,-4.5|1,-9>(18,-9,F:8,D:4,C:4,1,-84,(18,-4.5,1,-178)*)
			// C= -(D  + F:4 + F:4:4)
			// 00C4:
			//
			// sortBurst.max1 is unsound because all bits might be zero
			// nMaxShort = nominal 7.75
			if (   ((cBits[0] + cBits[1] + (cBits[0]>>4) + (cBits[1]>>4))&15) == 0
				&& sortOn.max1*5 < nMaxShort
				&& lead_in( .7*27/7.75*nMaxShort, 1.3*27/7.75*nMaxShort,
						   1.3*18/7.75*nMaxShort, .7*9/7.75*nMaxShort, nMaxShort ) )
			{
				strcpy(pProtocol, "G.I. Cable");
				// GD 2009 Code modified to report ditto count
				/*	
				if (   pFrameEnd[1] < sortOn.max1*5		// Nominal 18:1
					|| pFrameEnd[2] > nMaxShort		// Nominal 4.5 : 225/32
					|| pFrameEnd[2]*2 < nMaxShort	// Nominal 9 : 225/32
					|| pFrameEnd[3] > sortOn.max1*1.5)	// Nominal 1:1
				{
					strcat(pProtocol, "{1}");		// Repeat frame missing
				}
				*/
				// GD 2009 Start
				nDittos = 0;
				while (	pFullLimit > pFrameEnd + 4
					&&	pFrameEnd[1] > sortOn.max1*5		// Nominal 18:1
					&&	pFrameEnd[2] < nMaxShort			// Nominal 4.5 : 225/32
					&&	pFrameEnd[2]*2 > nMaxShort			// Nominal 9 : 225/32
					&&	pFrameEnd[3] < sortOn.max1*1.5		// Nominal 1:1
					&&	pFrameEnd[4] > pFrameEnd[1]*5	)	// Nominal about 1:10
				{
					nDittos++;
					pFrameEnd += 4;
					nFrameL += 2;
				}
				if ( nDittos == 0 )
				{
					strcat(pProtocol, "{1}");		// Repeat frame missing
				}
				// GD 2009 End
				*pOBC = cBits[0];
				*pHex = msb(*pOBC);
				*pDevice = cBits[1]&15;
				return;
			}

			// Dish_Network {57.6k,400}<1,-7|1,-4>(1,-15,(F:-6,U:5,D:5,1,-15)+)
			// 0002
			if (   sortOn.max1*3 < sortBurst.min1				// Nominal 1:5 (Rules out JVC)
				&& sortBurst.min1*2 > sortBurst.max1			// Nominal 5:8
				&& nFrame0 > nFrameLimit
				&& nFrame0 < sortOff.max1*3 )
			{
				// Set pLead to point to the initial (1, -15) lead-in so that
				// start is reported correctly by decode2()
				if (!nFrameCount && pFrame >= pDuration + 2) pLead = pFrame - 2;
				nNote_out = 2;
				strcpy(pProtocol,"Dish_Network");
				pHex[0] = msb(255-cBits[0]);
				*pDevice = 31-getLsb(11,5);
				*pSubDevice = 31-getLsb(6,5);
				*pOBC = 63-msb(cBits[0],6);
				return;
			}

			// Mitsubishi  {32.6k,300}<1,-3|1,-7>(D:8,F:8,1,-80)+
			// 0014: {~D:8}[~F:8]
			//
			if (   sortOn.max1*2.8 < sortBurst.min1				// Nominal 1:4 (usually rules out JVC, Bose)
				&& nFrame0 > sortBurst.max1*4
				&& sortOff.min1 > 750.)
			{
				strcpy(pProtocol, "Mitsubishi");
				*pDevice = cBits[0];
				*pOBC = cBits[1];
				*pHex = 255 - msb(*pOBC);
				return;
			}

			// JVC  {38k,525}<1,-1|1,-3>(16,-8,(D:8,F:8,1,-45)+)
			// 0034:
			//
			if (   sortOn.max1*5 > sortOff.max1
				&& nMaxShort < 2200. )
			{
				// nMaxShort = nominal 3
				int bL = lead_in( 2.5*nMaxShort, 13.*nMaxShort, 12.*nMaxShort, 2.*nMaxShort, nMaxShort );
				if (bL || nFrame0 > sortBurst.max1*4 )
				{
					strcpy(pProtocol, "JVC");
					// Adding {2} to JVC now handled by Decode2() so that JVC repeats do not show
					// separately as JVC{2} signals
					bInitLeadIn = bL;
					nNote_out = 2;
					/*
					if ( !bL )
					{
						strcat(pProtocol, "{2}");	// LeadIn missing
					}
					*/
					*pDevice = cBits[0];
					*pOBC = cBits[1];
					*pHex = 255 - msb(*pOBC);
					return;
				}
			}

			//GI RG  {38.5k,1000,msb}<1,-1|1,-3>(5,-3,A:16,1,-47.0m) 
			// SIM2 {38.8k,400}<3,-3|3,-7>(6,-7,D:8,F:8,3,-60m)
			//
			if (sortOff.max1  > sortBurst.max1*0.56 && sortOff.max1 < 3500. && sortOff.max1 > 2300.) {
				if (  sortOn.max2 < 1500. && sortOn.min1 > 1100.) {
					strcpy(pProtocol, "SIM2");
					*pDevice = cBits[0];
					*pOBC = cBits[1];
					newPreemptValue = prBySIM2;    // Avoid Async decode
					newPreemptLength = 16;
					return;
				}
				else if ( sortOn.max2 <= 1100. && sortOn.min1 > 800.) {
					strcpy(pProtocol, "GI RG");
					makeMsb();
					*pDevice = cBits[1];
					*pSubDevice = cBits[0] & 0x03;
					*pOBC = cBits[0] >> 2;
					newPreemptValue = prByGI_RG;    // Avoid Async decode
					newPreemptLength = 16;
					return;
				}
			}
            // Reject Samsung36 {38k,500}<1,-1|1,-3>(9,-9,D:8,S:8,1,-9,E:4,F:8,-68u,~F:8,1,-118)+
            //
			float midBurst = pFrameEnd[-1] + pFrameEnd[0]; // nominal 10
            if (   lead_in( .7*10/10*midBurst, // The initial 9,-9 must be >= 70% of a nominal 10
                          1.3*18/10*midBurst,  // The initial 9,-9 must be <= 130% of a nominal 18
                          1.3*9/10*midBurst,   // The initial 9 must be <= 130% of a nominal 9
                          .7*9/10*midBurst,    // The initial -6 must be >= 70% of a nominal 6
                          -midBurst )  )       // framing gap
            {
				return;
            }
        }
		break;
	case 17:
		{
			// Proton {38k,500}<1,-1|1,-3>(16,-8,D:8,1,-8,F:8,1,^63m)+
			// 005C
			if (nSpecial & Special_Proton)
			{
				strcpy(pProtocol, "Proton");
				*pDevice = cBits[0];
				*pOBC = getLsb(9,8);
				*pHex = 255 - msb(*pOBC);
				return;
			}
            // Konka {38k,500,msb}<1,-3|1,-5>(6,-6,D:8,F:8,1,-8,1,-46)+
			// 019B
			if (nSpecial & Special_Konka)
			{
				strcpy(pProtocol, "Konka");
				*pDevice =msb( cBits[0] );
				*pOBC = 
				*pHex = msb( cBits[1] );
				return;
			}
			if ( (nSpecial & Special_Feelux) && cBits[0]+cBits[1]==255 )
			{
				strcpy(pProtocol, "Feelux");
				*pOBC = 
				*pHex = msb( cBits[0] );
				return;
			}
		}
		break;
	case 19:
		{
		// Metz19 {37.9K,106,msb}<4,-9|4,-16>(8,-22,T:1,D:3,~D:3,F:6,~F:6,4,-125m)+  DAR March 2011
		// Metz factory spec {37.9K,26.4u,msb}<16,-37|16,-63.5>(32,-89,T:1,D:3,~D:3,F:6,~F:6,4,-125m)+
			if ( pLead && pLead[0] > sortOn.max1*1.5  && pLead[1] > sortOff.max1 )
			{
				unsigned int dev=getLsb(1,3);
				unsigned int fun = getLsb(7,6);
				if ((dev ^ getLsb(4,3)) ==  0x7 && (fun ^ getLsb(13,6)) == 0x3F) 
				{
					strcpy(pProtocol,"Metz19");
					*pDevice = msb(dev) >> 5;
					*pOBC = msb(fun) >> 2;
					sprintf(pMisc, "T=%d", cBits[0] & 1);
					pHex[0] = *pOBC;
					return;
				}
			}
		}
		break;
	case 20:
		{
			// Samsung20 {38.4k,564}<1,-1|1,-3>(8,-8,D:6,S:6,F:8,1,^???)+
			//
			// nMaxShort = nominal 3
			if ( lead_in( .7*8/3*nMaxShort, 1.2*16/3*nMaxShort, 1.2*8/3*nMaxShort, .7*8/3*nMaxShort, nMaxShort ) )
			{
				strcpy(pProtocol, "Samsung20" );
				*pDevice = getLsb(0,6);
				*pSubDevice = getLsb(6,6);
				*pOBC = getLsb(12,8);
				*pHex = msb(*pOBC);
				return;
			}
		}
		break;
	case 22:
		{
			// Panasonic_Old {57.6k,833}<1,-1|1,-3>(4,-4,D:5,F:6,~D:5,~F:6,1,-???)+
			// 0000: {1:~D0:6, 1:~D1:6, 1:~D2:6}[F:6 + <3|2|1>]
			//
			// nMaxShort = nominal 3
			if ( lead_in( .6*8/3*nMaxShort, 1.3*8/3*nMaxShort, 1.5*4/3*nMaxShort, .6*4/3*nMaxShort, -3000 ) )
			{
				unsigned int dev = getLsb(0,5);
				unsigned int fun = getLsb(5,6);
				if ( 31-getLsb(11,5)==dev && 63-getLsb(16,6)==fun )
				{
					strcpy(pProtocol, "Panasonic_Old");
					*pDevice = dev;
					*pOBC = fun;
					int hex = msb(fun);
					pHex[0] = hex+3;
					pHex[1] = hex+2;
					pHex[2] = hex+1;
					return;
				}
			}
		}
		break;
	case 24:
		{
			// Kathrein (pid-0066)	GD 2009 changed repeat from + to *
			// as not all signals in SAT/0658 (with pid 0066) are repeating
			// {38k,540}<1,-1|1,-3>(16,-8,D:4,~D:4,F:8,~F:8,1,^105m,(16,-8,F:8,1,^105m)*)
			// Dgtec (? pid_016A)
			// {38k,560}<1,-1|1,-3>(16,-8,D:8,F:8,~F:8,1,^108m,(16,-4,1,^108m)+)
			// Logitech PS3 Adapter  DAR 2012 added
			// {38k,127}<3,-4|3,-8>(31,-36,D:4,~D:4,F:8,~F:8,3,-50.2m)+
			// nMaxShort = nominal 3
			if (    0xFF - getLsb(8,8) == getLsb(16,8)
				 && sortBurst.mid1 > .8*.5*sortBurst.mid2
				 && lead_in( .7*8/3*nMaxShort, 1.3*24/3*nMaxShort, 1.3*16/3*nMaxShort, .7*8/3*nMaxShort, nMaxShort ) )
			{
				if (   0x3F-getLsb(6,6) != getLsb(18,6)       // If it doesn't fit more common 24 bit protocols
					||                                        // or it fits a stricter timing standard
						  sortBurst.mid1 < 1.2*2*560
					   && lead_in( .8*16/3*nMaxShort, 1.2*24/3*nMaxShort, 1.2*16/3*nMaxShort, .8*8/3*nMaxShort, nMaxShort ) )
				{
					nDittos = 0;	// GD 2009
					*pOBC = getLsb(8,8);
					pHex[0] = msb(255-*pOBC,8);
					if ( 0xF - getLsb(0,4) == getLsb(4,4) )
					{
						if (1.78*sortBurst.mid1 > sortBurst.mid2)
						{
							strcpy(pProtocol, "Logitech" );	// DAR 2012 
							*pDevice = getLsb(0,4);
						}
						else
						{
							strcpy(pProtocol, "Kathrein" );	// GD 2009 changed name from pid-0066
							*pDevice = getLsb(0,4);
							// GD 2009 Start
							pBit += 4;
							while (		pFullLimit > pFrameEnd + 20
									&&	pFrameEnd[1] > pFrameEnd[2]*1.25	// Nominal 2:1
									&&	pFrameEnd[2] > sortOff.max1*2		// Nominal 8:3
									&&	pFrameEnd[2] < sortOff.max1*4		// Nominal 8:3
									&&	pFrameEnd[20] > pFrameEnd[1]*4 )
							{
								decodeX2(8);
								nBit -= 8;
								pBit -= 16;
								if ( getLsb(8,8) != getLsb(24,8) ) break;
								nDittos++;
								pFrameEnd += 20;
								nFrameL += 10;
							}
							if ( nDittos == 0 )
							{
								nDittos = -1;					// prevent multiple frames being counted
								bSuppressCount = true;			// prevent "no repeat" being inserted
								strcpy(pMisc, "non-repeating");	// Kathrein has specifically non-repeating signals
							}
							// GD 2009 End
						}
					}
					else
					{
						strcpy(pProtocol, "Dgtec" );
						*pDevice = getLsb(0,8);
						// GD 2009 Start
						while (		pFullLimit > pFrameEnd + 4
								&&	pFrameEnd[1] > pFrameEnd[2]*2	// Nominal 4:1
								&&	pFrameEnd[2] > sortOff.max1		// Nominal 4:3
								&&	pFrameEnd[2] < sortOff.max1*2	// Nominal 4:3
								&&	pFrameEnd[3] < nMaxShort
								&&	pFrameEnd[4] > pFrameEnd[1]*4 )
						{
							nDittos++;
							pFrameEnd += 4;
							nFrameL += 2;
						}
						if ( nDittos == 0 ) nDittos = -1;	// prevent multiple frames being counted
						// GD 2009 End
					}
					return;
				}
			}
			if ( 0x3F-getLsb(6,6) == getLsb(18,6) )
			{
				// ScAtl-6 {57.6k,846}<1,-1|1,-3>(4,-4,D:6,F:6,~D:6,~F:6,1,-40)+
				// 0078:
				//
				// Emerson {36.7k,872}<1,-1|1,-3>(4,-4,D:6,F:6,~D:6,~F:6,1,-39)+
				// 0065:
				//
				// Sampo {38.4k, 833}<1,-1|1,-3>(4,-4,D:6,F:6,S:6,~F:6,1,-39)+
				//
				// nMaxShort = nominal 3

				if (   sortOn.max1 > 600
					&& lead_in( .6*8/3*nMaxShort, 1.3*8/3*nMaxShort, 1.3*4/3*nMaxShort, .7*4/3*nMaxShort, nMaxShort ) )
				{
					if ( getLsb(0,6)==63 || 0x3F-getLsb(0,6) != getLsb(12,6) )
					{
						strcpy(pProtocol, "Sampo" );
						*pDevice = cBits[0] & 63;
						*pSubDevice = getLsb(12,6);
						*pOBC = getLsb(6,6);
						return;
					}
					if (nFreq>47000)
					{
						strcpy(pProtocol, "ScAtl-6" );
						*pDevice = cBits[0] & 63;
						*pOBC = getLsb(6,6);
						pHex[0] = msb(cBits[2],9)&0xFE;
						return;
					}
					strcpy(pProtocol, "Emerson" );
					*pDevice = cBits[0] & 63;
					*pOBC = getLsb(6,6);
					pHex[2] = 1+ ( pHex[1] = 1+ ( pHex[0] = msb(63-*pOBC,8) ) );
					return;
				}
				// RCA(Old) IRP revised by GD to accord with protocol 002D, as it was not being recognised by DecodeIR.
				// Lead-out burst changed from 1,-15 to 2,-16.
				// RCA(Old) {58k,460,msb}<1,-2|1,-4>(40,-8,D:4,F:8,~D:4,~F:8,2,-16,(8,-8,D:4,F:8,~D:4,~F:8,2,-16)+)
				// 002D:
				// RCA {58k,460,msb}<1,-2|1,-4>(8,-8,D:4,F:8,~D:4,~F:8,1,-15)+
				// 00AF:
				// 0114:
				// RCA-38 added by GD 2009
				// RCA-38 {38.7k,460,msb}<1,-2|1,-4>(8,-8,D:4,F:8,~D:4,~F:8,1,-15)+

				// nMaxShort = nominal 4
				if (   0x3F-getLsb(0,6) == getLsb(12,6)
					&& lead_in( .55*16/4*nMaxShort, 1.3*48/4*nMaxShort, 1.3*40/4*nMaxShort, .7*8/4*nMaxShort, -40./4*nMaxShort ) )
				{
					int nDev = cBits[0] & 15;
					int nFun = getLsb(4,8);
					strcpy(pProtocol,"RCA");
					if ( pLead[0] > sortOff.max1*4 )
						bInitLeadIn = true;
					else
						bInitLeadIn = false;
//						strcat(pProtocol, "{1}");
					if ( nFreq < 48000 )
						strcat(pProtocol, "-38");   // GD 2009 New 38k version of RCA protocol
					*pDevice = msb(nDev,4);
					*pOBC = msb(nFun,8);
					pHex[0] = *pOBC;
					return;
				}
			}
		}
		break;
	case 32:
	// Anthem {38.0k,605}<1,-1|1,-3>((8000u,-4000u,D:8,S:8,F:8,C:8,1,-25m)3, -75m)+
		{
			if (   pLead
				&& ((cBits[0] + cBits[1] + cBits[2] + cBits[3]) & 0xFF ) == 0 
				&& pDuration[67] < 30000 && pDuration[67] > 20000)
			{
				strcpy(pProtocol, "Anthem" );
				*pDevice = cBits[0];
				*pSubDevice = cBits[1];
				*pOBC = cBits[2];
				return;
			}
			// NECx1 {38.4k,564}<1,-1|1,-3>(8,-8,D:8,D:8,F:8,~F8,1,^108m,(8,-8,D:1,1,^108m)*)
			// NECx2 {38.4k,564}<1,-1|1,-3>(8,-8,D:8,D:8,F:8,~F8,1,^108m)+
			// NEC1 {38.4k,564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,1,-78,(16,-4,1,-173)*)
			// NEC2 {38.4k,564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,1,-78)+
			// 000C: {?,?,?} [?]
			// 005A: {?, ~D:8, ~S:8}[~F:8]
			// 00B6:
			// 005B??
			// Pioneer {40k,564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,1,-78)+
			// 005F:
			// 006A:
			// 007E:
			// 00E2:
			// Apple IRP notation: {38.4k,564}<1,-1|1,-3>(16,-8,D:8,S:8,C:1,F:7,I:8,1,-78,(16,-4,1,-173)*)
			// C=1 if the number of 1 bits in the fields F and I is even
			// 01E0:
			if (   pLead
				&& cBits[0] == 133
				&& cBits[1] == 48
				&& (cBits[2] ^ cBits[3]) >= 240 )
			{
				strcpy(pProtocol, "Tivo" );
				*pDevice = cBits[0];
				*pSubDevice = cBits[1];
				*pOBC = cBits[2];
				*pHex = msb(255-cBits[2]);
				sprintf(pProtocol+strlen(pProtocol), " unit=%d", cBits[3] & 15);
				return;
			}
			if (   pLead       //DAR May 2011
				&& ((cBits[0] == 238) || (cBits[0] == 224)) 
				&& cBits[1] == 135   
				&& (parity(getMsb(17,15)) != getMsb(16,1)) ) // parity() returns odd parity
			{
				strcpy(pProtocol, "Apple" );
				if (cBits[0] == 14) strcat(pProtocol,"--pairing");
				*pDevice = cBits[0];
				*pSubDevice = cBits[1];
				*pOBC = cBits[2] >> 1;
				*pHex = msb(255-cBits[2]);
				sprintf(pMisc, "PairID=%d", cBits[3]);
				return;
			}
			strcpy(pSuffix, "" );
			if (   pLead  )     
			{
				switch (cBits[2] ^ cBits[3]) //DAR Nov 2010 start
				{
					case 0xFF: strcpy(pSuffix, "" ); break;
					case 0x7F: strcpy(pSuffix, "-y1" ); break;
					case 0xFE: strcpy(pSuffix, "-y2" ); break;
					case 0x7E: strcpy(pSuffix, "-y3" ); break;
					default: 
						{
							unsigned char hiNib2 = cBits[2] >> 4;  //DAR May 2012  added rnc (SlingCatcher)
							unsigned char loNib2 = cBits[2] & 0x0F;
							unsigned char hiNib3 = cBits[3] >> 4;
							unsigned char loNib3 = cBits[3] & 0x0F;
							if ( ((hiNib2 ^ loNib3) == 0x0F) &&  ((loNib2 ^ hiNib3) == 0x0F) )
								strcpy(pSuffix, "-rnc" );
							else {
								strcpy(pSuffix, "-f16" );
								sprintf(pMisc, "OBC2=%d", cBits[3]);
							}
							//sprintf(pMisc, "%X", cBits[0]);
							//for (int ndx=1; ndx<4; ndx++)
							//sprintf(pMisc+strlen(pMisc), ".%X", cBits[ndx]);
						}
				}
				/*** && 255-cBits[2] == cBits[3] )  replaced by the above Nov 2010 DAR  ***/
				strcpy(pProtocol, "NEC" );
				bool foundX = false;
				nDittos = 0;	// GD 2009 Number of "ditto" repeats
				float one = (cBits[0]&1) ? (sortOff.max1*.25) : (sortOff.max1*.75);	// GD 2009 moved from below

				if ( lead_in( .8*8/3*nMaxShort, 1.2*16/3*nMaxShort, 1.2*8/3*nMaxShort, .8*8/3*nMaxShort, nMaxShort ) )
				{	// GD 2009 - Changes made to enable reporting of "ditto" repeats
					/* if */ while ( pFullLimit > pFrameEnd + 6	// GD 2009 Added test on pFullLimit  
						&&	pFrameEnd[1] + pFrameEnd[2] < 6*sortOff.max1
						&&	pFrameEnd[1] + pFrameEnd[2] > 4*sortOff.max1
						&&	pFrameEnd[2] > 2*sortOff.max1
						&&	pFrameEnd[3] < nMaxShort /* )
					{
						float one = (cBits[0]&1) ? (sortOff.max1*.25) : (sortOff.max1*.75);
						if ( */	// GD 2009  Added && to concatenate with above conditionals 
							&& pFrameEnd[4] > one
							&& pFrameEnd[4] < 2*one
							&& pFrameEnd[5] < nMaxShort
							&& pFrameEnd[6] > pFrameEnd[2]*4 )
						// GD 2009 Start
						{	
							nDittos++;
							pFrameEnd += 6;
							nFrameL += 3;
						}

						if ( nDittos )
						// GD 2009 End
						{
							strcat(pProtocol,"x1");
							strcat(pProtocol,pSuffix);
							foundX = true;
						}
						else if ( pFrameEnd[0] > 40000 )
						{
							strcat(pProtocol,"x");
							foundX = true;
						}
					/*	}	*/
				}
				if ( ! foundX )
				{	// GD 2009 - Changes made to enable reporting of "ditto" repeats
					/* if */ while ( pFullLimit > pFrameEnd + 4   // GD 2009 Added teston pFullLimit
						&& pFrameEnd[1] > pFrameEnd[2]*5/4
						&& pFrameEnd[2] > sortOff.max1
						&& pFrameEnd[2] < sortOff.max1*2
						&& pFrameEnd[3] < nMaxShort
						&& pFrameEnd[4] > pFrameEnd[1]*2 )
					// GD 2009 Start
					{	
						nDittos++;
						pFrameEnd += 4;
						nFrameL += 2;
					}

					if ( nDittos )
					// GD 2009 End
					{
						strcat(pProtocol,"1");
						strcat(pProtocol,pSuffix);
					}
					else if ( nFreq > 39700 && nFreq < 41000 )
					{
						strcpy(pProtocol, "Pioneer" );
						goto no2;
					}
				}
				/*	Superseded by more robust processing in decode2()
				if (pFrame >= pDuration+2*nSingle)
				{
					strcat(pProtocol,"2");
				}
				*/
no2:            *pDevice = cBits[0];
				if (cBits[0]+cBits[1] != 255)
					*pSubDevice = cBits[1];
				*pOBC = cBits[2];
				*pHex = msb(cBits[3]);
				return;
			}
		}
		break;
    case 37:
        {
			if ( (nSpecial & Special_Samsung36) && getLsb(21,8)+getLsb(29,8)==255 )
			{
				strcpy(pProtocol, "Samsung36");
				*pDevice =cBits[0];
				*pSubDevice = cBits[1];
				*pOBC = getLsb(21,8);
				*pHex = msb( *pOBC );
                sprintf(pMisc, "E=%d", getLsb(17,4));
	            newPreemptValue = prBySamsung36; 	// Avoid Gap decode of partial frame
	            newPreemptLength = nFrameL;
				return;
			}
        }
        break;
	case 40:  // Added by GD 2009
		{
			// IODATAn		{38k,550}<1,-1|1,-3>(16,-8,0:7,D:7,S:7,0:7,F:8,C:4,1,^108m)+
			// IODATAn-x-y	{38k,550}<1,-1|1,-3>(16,-8,x:7,D:7,S:7,y:7,F:8,C:4,1,^108m)+
			// n = F:4 ^ F:4:4 ^ C:4
			// Only known example of this protocol is IODATA1
			if ( pLead )
			{
				unsigned int fun = getLsb(28,8);
				unsigned int x = getLsb(0,7);
				unsigned int y = getLsb(21,7);
				sprintf( pProtocol, (x || y) ? "IODATA%d-%d-%d" : "IODATA%d", (fun ^ (fun >> 4) ^ getLsb(36,4)) & 15, x, y );
				*pDevice = getLsb(7,7);
				*pSubDevice = getLsb(14,7);
				*pOBC = fun;
				*pHex = msb(fun);
				return;
			}
		}
		break;
	case 42:
		{
			// Aiwa {38k,550}<1,-1|1,-3>(16,-8,D:8,S:5,~D:8,~S:5,F:8,~F:8,1,-42,(16,-8,1,-165)*)
			// 005E: {D:-8,S:-5}[F:-8]
			// 009E: {~S:-5}[~D:-8,~F:-8]
			//
			if ( pLead )
			{
				unsigned int dev = cBits[0];
				unsigned int sub = getLsb(8,5);
				unsigned int fun = getLsb(26,8);
				if (   255-getLsb(13,8) == dev
					&&  31-getLsb(21,5) == sub
					&& 255-getLsb(34,8) == fun )
				{
					strcpy(pProtocol, "Aiwa");
					*pDevice = dev;
					*pSubDevice = sub;
					*pOBC = fun;
					*pHex = msb(fun);
					// GD 2009 Start
					nDittos = 0;
					while (		pFullLimit > pFrameEnd + 4
							&&	pFrameEnd[1] > pFrameEnd[2]*1.25	// Nominal 2:1
							&&	pFrameEnd[2] > sortOff.max1*2		// Nominal 8:3
							&&	pFrameEnd[2] < sortOff.max1*4		// Nominal 8:3
							&&	pFrameEnd[3] < nMaxShort
							&&	pFrameEnd[4] > pFrameEnd[1]*4 )
					{
						nDittos++;
						pFrameEnd += 4;
						nFrameL += 2;
					}
					if (nDittos == 0) nDittos = -1;  // prevent multiple frames being counted
					// GD 2009 End
					return;
				}
			}
		}
		break;
	case 48:
		{
			// 48-NEC {564}<1,-1|1,-3>(16,-8,D:8,S:8,F:8,~F:8,E:8,~E:8,1,-??,(16,-4,1,-??)*)
			if (   255-cBits[2]==cBits[3] && 255-cBits[4]==cBits[5]
				&& pLead )
			{
				strcpy(pProtocol,"48-NEC");
				if (   pFrameEnd[1] > pFrameEnd[2]
					&& pFrameEnd[2] > sortOff.max1
					&& pFrameEnd[2] < sortOff.max1*2
					&& pFrameEnd[3] < nMaxShort)
				{
					strcat(pProtocol,"1");
				}
				/*	Superseded by more robust processing in decode2()
				if (pFrame >= pDuration+2*nSingle)
					strcat(pProtocol,"2");
				*/
				*pDevice = cBits[0];
				*pSubDevice = cBits[1];
				*pOBC = cBits[2];
				pHex[0] = msb(255-cBits[2]);
				// GD 2009 Start
				nDittos = 0;
				while (		pFullLimit > pFrameEnd + 4   
						&&	pFrameEnd[1] > pFrameEnd[2]*5/4
						&&	pFrameEnd[2] > sortOff.max1
						&&	pFrameEnd[2] < sortOff.max1*2
						&&	pFrameEnd[3] < nMaxShort
						&&	pFrameEnd[4] > pFrameEnd[1]*2 )
				{	
					nDittos++;
					pFrameEnd += 4;
					nFrameL += 2;
				}
				// GD 2009 End
				sprintf(pMisc, "E=%d", cBits[4]);
				return;
			}

			//  X = M:4:0 ^ M:4:4 ^ N:4:0 ^ N:4:4
			//  C = D ^ S:4:0 ^ S:4:4 ^ F:4:0 ^ F:4:4 ^ E
			//  T = D + S:4:0 + S:4:4 + F:4:0 + F:4:4
			//
			// Fujitsu  {38k,400}<1,-1|1,-3>(8,-4,20:8,99:8, X:4,E:4,D:8,S:8,F:8,1,-110)+
			// Kaseikyo {37k,432}<1,-1,1,-3>(8,-4,M:8,N:8,   X:4,D:4,S:8,G:8,F:8,1,-173)+
			// SharpDVD {38k,400}<1,-1|1,-3>(8,-4,170:8,90:8,X:4,D:4,S:8,F:8,E:4,C:4,1,-48)+
			// 00F8
			//
			// Panasonic {37k,432}<1,-1,1,-3>(8,-4, 2:8,32:8,D:8,S:8,F:8,(D^S^F):8,1,-173)+
			// JVC-48    {37k,432}<1,-1,1,-3>(8,-4, 3:8, 1:8,D:8,S:8,F:8,(D^S^F):8,1,-173)+
			// Denon-K   {37k,432}<1,-1,1,-3>(8,-4,84:8,50:8,0:4,D:4,S:4,F:12,C:8,1,-173)+
			// 001F:
			// 00C9:
			// 00CD:
			//
			// Teac-K {37.9k,432}<1,-1,1,-3>(8,-4,67:8,83:8,X:4,D:4,S:8,F:8,T:8,1,-100,(8,-8,1,-100)+
			// 
			// Mitsubishi-K {35k,432}<1,-1,1,-3>(8,-4,35:8,203:8,X:4,D:8,S:8,F:8,T:4,1,-100)+
			// 
			if ( lead_in( .8*5/3*nMaxShort, 1.2*12/3*nMaxShort, 1.2*8/3*nMaxShort, .8*4/3*nMaxShort, nMaxShort )  )
			{
				int xor1 = cBits[0] ^ cBits[1];
				if ( ( ( (xor1>>4) ^ xor1 ^ cBits[2] ) & 15 ) == 0 )
				{
					int xor2 = cBits[3] ^ cBits[4] ^ cBits[5];
					if (xor2 == cBits[2])
					{
						if (cBits[0] == 2 && cBits[1] == 32)
						{
							strcpy(pProtocol, "Panasonic");
							*pDevice = cBits[2];
							*pSubDevice = cBits[3];
							*pOBC = cBits[4];
							*pHex = msb(255-cBits[4]);
							return;
						}
						if (cBits[0] == 3 && cBits[1] == 1)
						{
							strcpy(pProtocol, "JVC-48");
							*pDevice = cBits[2];
							*pSubDevice = cBits[3];
							*pOBC = cBits[4];
							*pHex = msb(255-cBits[4]);
							return;
						}
						if (cBits[0] == 84 && cBits[1] == 50)
						{
							strcpy(pProtocol, "Denon-K");
							*pDevice = cBits[2]>>4;
							*pSubDevice = cBits[3]&15;
							*pOBC = getLsb(28,12);
							pHex[0] = msb(255-cBits[3]); 
							pHex[1] = msb(255-cBits[4]);
							return;
						}
					}
					int x2 = cBits[2]>>4;
					if (cBits[0] == 20 && cBits[1] == 99)
					{
						strcpy(pProtocol, "Fujitsu");
						*pDevice = cBits[3];
						if ( cBits[4] != cBits[3] )
							*pSubDevice = cBits[4];
						*pOBC = cBits[5];
						pHex[0] = msb(255-cBits[4]); 
						pHex[1] = msb(255-cBits[5]);
						if (x2)
							sprintf(pMisc,"E=%d",x2);
						return;
					}
					if (cBits[0] == 170 && cBits[1] == 90)  //DAR Dec 2010
					{
						strcpy(pProtocol, "SharpDVD");
						*pDevice = x2;
						*pSubDevice = cBits[3];
						*pOBC = cBits[4];
						pHex[0] = msb(255-cBits[4]); 
						pHex[1] = msb(255-cBits[5]);
						int x5 = cBits[5]&0xF;
						sprintf(pMisc,"E=%d",x5);
						return;
					}
					if (cBits[0] == 67 && cBits[1] == 83)
					{
						int sum = (cBits[2]>>4) + (cBits[3]&15) + (cBits[3]>>4) + (cBits[4]&15) + (cBits[4]>>4);
						if (sum == cBits[5])
						{
							strcpy(pProtocol, "Teac-K");
							*pDevice = cBits[2]>>4;
							*pSubDevice = cBits[3];
							*pOBC = cBits[4];
							pHex[0] = msb(255-cBits[4]);
							pHex[1] = msb(255-cBits[5]);

							// GD 2009 Start
							nDittos = 0;
							while (		pFullLimit > pFrameEnd + 4   
									&&	pFrameEnd[1] > pFrameEnd[2]*0.5
									&&	pFrameEnd[2] > sortOff.max1
									&&	pFrameEnd[2] < sortOff.max1*2
									&&	pFrameEnd[3] < nMaxShort
									&&	pFrameEnd[4] > pFrameEnd[1]*4 )
							{	
								nDittos++;
								pFrameEnd += 4;
								nFrameL += 2;
							}
							if ( nDittos == 0 ) nDittos = -1;  // prevent multiple frames being counted
							// GD 2009 End
							return;
						}
					}
					if (cBits[0] == 35 && cBits[1] == 203)
					{
						int sum = (cBits[3]>>4) + (cBits[4]&15) + (cBits[4]>>4) + (cBits[5]&15) + (cBits[5]>>4);
						if ( (sum&15) == 15 )
						{
							strcpy(pProtocol, "Mitsubishi-K");
							*pDevice = getLsb(20,8);
							*pSubDevice = getLsb(28,8);
							*pOBC = getLsb(36,8);
							return;
						}
					}
					/*  removed Jan 2015 DAR
					if ( ( ( (xor2>>4) ^ xor2 ^ x2 ) & 15 ) == 0 )
					{
						sprintf(pProtocol, "Kaseikyo-%d.%d", cBits[0], cBits[1] );
						*pDevice = cBits[2]>>4;
						*pSubDevice = cBits[3];
						*pOBC = cBits[4];
						*pHex = msb(255-cBits[4]);
						sprintf(pMisc,"E=%d", cBits[5]&15 );
						return;
					}
					*/

			// Kaseikyo {37k,432}<1,-1,1,-3>(8,-4,M:8,N:8,X:4,D:4,S:8,F:8,G:8,1,-173)+
			// General Kaseikyo, checksum or OEM unknown    Promoted to main Kaseikyo decode Jan 2015 DAR
					sprintf(pProtocol, "Kaseikyo");
					*pDevice = cBits[2]>>4;
					*pSubDevice = cBits[3];
					*pOBC = cBits[4];
					pHex[0] = msb(255-cBits[4]);
					pHex[1] = msb(255-cBits[5]);
					sprintf(pMisc,"M=%d N=%d G=%d", cBits[0], cBits[1], cBits[5] );
					return;
				}
			}
		}
		break;
	case 56:
		{
			// Panasonic2 {37k,432}<1,-1,1,-3>(8,-4,2:8,32:8,D:8,S:8,X:8,F:8,(D^S^X^F):8,1,-173)+
			if ( lead_in( .8*5/3*nMaxShort, 1.2*12/3*nMaxShort, 1.2*8/3*nMaxShort, .8*4/3*nMaxShort, nMaxShort )  )
			{
				int xor1 = cBits[0] ^ cBits[1];
				int xor2 = cBits[3] ^ cBits[4] ^ cBits[5] ^ cBits[6];
				int x2 = cBits[2]>>4;
				if ( ( ( (xor1>>4) ^ xor1 ^ cBits[2] ) & 15 ) == 0 )
				{
					if (xor2 == cBits[2])
					{
						if (cBits[0] == 2 && cBits[1] == 32)
						{
							strcpy(pProtocol, "Panasonic2");
							*pDevice = cBits[2];
							*pSubDevice = cBits[3];
							sprintf(pMisc,"X=%d", cBits[4]);
							*pOBC = cBits[5];
							*pHex = msb(255-cBits[5]);
							return;
						}
						if (cBits[0] == 3 && cBits[1] == 1)
						{
							strcpy(pProtocol, "JVC-56");
							*pDevice = cBits[2];
							*pSubDevice = cBits[3];
							sprintf(pMisc,"X=%d", cBits[4]);
							*pOBC = cBits[5];
							*pHex = msb(255-cBits[5]);
							return;
						}
					}
					if (cBits[0] == 20 && cBits[1] == 99)
					{
						strcpy(pProtocol, "Fujitsu-56");
						*pDevice = cBits[3];
						if ( cBits[4] != cBits[3] )
							*pSubDevice = cBits[4];
						sprintf(pMisc,"X=%d", cBits[5]);
						*pOBC = cBits[6];
						*pHex = msb(255-cBits[6]);
						if (x2)
							sprintf(pMisc+strlen(pMisc)," E=%d",x2);
						return;
					}
					//Kaseikyo56 {37k,432}<1,-1,1,-3>(8,-4,M:8,N:8,X:4,D:4,S:8,E:8,F:8,G:8,1,-173)+ Revised DAR Jan 2015
					sprintf(pProtocol, "Kaseikyo56");
					*pDevice = cBits[2]>>4;
					*pSubDevice = cBits[3];
					*pOBC = cBits[5];
					//*pHex = msb(255-cBits[5]);
					sprintf(pMisc,"M=%d N=%d E=%d G=%d", cBits[0], cBits[1], cBits[4], cBits[6] );
					return;
				}
			}
		}
		break;
	} // switch (nBit)

	if (preemptValue >= prGap)
		return;     // Don't do generic decode if some other decoder was confident
	
	if (bLeadIn)
		return;     // Don't do generic decode if this looks like LeadIn to some other signal

	if (sortOff.max1 > 15000)
		return;     // Kludge to supress decodes crossing frame boundaries


	if (   nBit >= 12
		&& (   nFrame0 > sortBurst.max1*2.
			|| (   pLead
				&& (   nFrame1 > (pLead[0]+pLead[1])*2.
					|| (   pLead2
						&& nFrame2 > (pLead2[0]+pLead2[1])*2. ) ) ) ) )
	{
		sprintf(pProtocol, "Gap-%.0f-%.0f-%d?", sortOn.max1, nMaxShort, nBit);
		if (nBit <= 8)
		{
			*pOBC = cBits[0];
		}
		else
		{
			*pOBC = getLsb(nBit-8, 8);
			if ( nBit <= 16 )
			{
				*pDevice = getLsb(0, nBit-8);
			}
			else
			{
				*pDevice = cBits[0];
				if ( nBit < 24 )
					*pSubDevice = getLsb(8, nBit-16);
				else
					*pSubDevice = cBits[1];
			}
		}
		int bytes = (nBit+7)>>3;
		sprintf(pMisc, "%X", cBits[0]);
		for (int ndx=1; ndx<bytes; ndx++)
			sprintf(pMisc+strlen(pMisc), ".%X", cBits[ndx]);
	}
} // tryGap

int Signal::phaseBit2()
{
	// On entry pBit points to the mid bit boundary of the prior bit
	// nState is the value of the prior bit:
    //   -1 =  LongHi/Lo
	//    0 = ShortHi/Lo
	//    1 =      Lo/Hi
	// Find the next mid bit boundary (current bit) and emit the decode of that bit.
	// If *pBit is short
	//    then *pBit represents just the second half of the prior bit
	//         and (*++pBit must be short) represents the first half
	//             of the current bit.
	// If *pBit is long
	//    then *pBit represents both the second half of the prior bit and the first
	//         half of the current bit.
	// In other cases return failure.
    // Returns
    //    1 = success
    //    0 = failure
    //   -1 = posible rc5x gap
	
	if ( nBit >= sizeof(cBits)*8 )
	{
		return 0;
	}
	if ( nState>0 )   // *pBit is pulse
	{
		double x = *pBit;
		if ( x < m_minShortPulse || x > m_maxLongPulse )
		{
			return 0;
		}
		if ( x > m_maxShortPulse )
		{
			m_extra = x - m_nominalLongPulse;
			nState = -1;
			++pBit;
			nBit++;
			return 1;
		}
		x += *++pBit - m_nominalShortPulse;
		if ( x < m_minShortGap )
		{
			return 0;
		}
		if ( x > m_maxShortGap )
		{
			if ( pBit == pFrameEnd )
			{
				return 1;	// Last bit was Lo/Hi and the frame ended on a bit boundary, so caller
							// went too far.  So return success, but don't emit a decoded bit.
			}
			return -1;
		}

        x -= m_nominalShortGap;
        if ( x > m_posDelta ) m_posDelta = x;
        if ( x < m_negDelta ) m_negDelta = x;

		cBits[nBit>>3] |= 1 << (nBit&7);
		++pBit;
		nBit++;
		return 1;
	}
	else            // *pBit is a gap
	{
		double x = *pBit + m_extra;
		if ( x < m_minShortGap )
		{
			return 0;
		}
		if ( x > m_maxLongGap )
		{
			return -1;
		}
		if ( x >= m_minLongGap )
		{
            x -= m_nominalLongGap;
            x *= (nState==0) ? (2./3.) : .5;
            if ( x > m_posDelta ) m_posDelta = x;
            if ( x < m_negDelta ) m_negDelta = x;

			nState = 1;
			cBits[nBit>>3] |= 1 << (nBit&7);
			++pBit;
			nBit++;
			return 1;
		}
		if ( x > m_maxShortGap )
		{
			return 0;
		}

        x -= m_nominalShortGap;
        if ( nState )
        {
            x *= (2./3.);
            nState = 0;
        }
        if ( x > m_posDelta ) m_posDelta = x;
        if ( x < m_negDelta ) m_negDelta = x;

		x = *++pBit;
		if ( x < m_minShortPulse || x > m_maxShortPulse )
		{
			return 0;
		}
		m_extra = x - m_nominalShortPulse;

		++pBit;
		nBit++;
		return 1;
	}
}

int Signal::phaseBit()
{
	// On entry pBit points to a mid bit boundary (prior bit)
	// Find the next mid bit boundary (current bit) and emit the decode of that bit.
	// If nMinShort <= *pBit <= nMaxShort
	//    then *pBit represents just the second half of the prior bit
	//         and ( nMinShort2 <= *++pBit <= nMaxShort2 ) represents the first half
	//             of the current bit.
	// If nMinLong <= *pBit <= nMaxLong
	//    then *pBit represents both the second half of the prior bit and the first
	//         half of the current bit.
	// In other cases return failure.

    float dur = *pBit;
	if (dur < nMinShort || dur > nMaxLong || nBit >= sizeof(cBits)*8)
    {
		return 0;
    }
	if (dur >= nMinLong)
	{
		nState = 1-nState;
	}
	else if (dur > nMaxShort )
	{
		return 0;
	}
    else
	{
        float dur2 = *++pBit;
		if (dur2 < nMinShort2 )
		{
            if (dur2 <= minGlitch)
            {
                if ( ++pBit < pFrameEnd )
                {
                    dur += dur2 + *pBit;
                    if ( dur >= nMinLong && dur <= nMaxLong )
                    {
                		nState = 1-nState;
                        goto ok;
                    }
                }
            }
			return 0;
		}
		if ( pBit == pFrameEnd )
			return 1;	// Last bit was Lo/Hi and the frame ended on a bit boundary, so caller
						// went too far.  So return success, but don't emit a decoded bit.
						// (When last bit is Hi/Lo the frame ends on a mid bit boundary,
						//  so the caller would know when to quit and we wouldn't get here).
		if ( dur2 > nMaxShort2 )
			return 0;
	}
ok:
	++pBit;
	cBits[nBit>>3] |= nState << (nBit&7);
	nBit++;
	return 1;
}

void Signal::tryRC5()
//
// RC5         {36k,msb,889}<1,-1|-1,1>(1:1,~F:1:6,T:1,D:5,F:6,^114m)+
// StreamZap  OLD {36k,msb,889}<1,-1|-1,1>(1:1,~F:1:6,T:1,D:6,F:6 // OLD changed to RC5-7F DAR Dec 2010 
// RC5-7F      {36k,msb,889}<1,-1|-1,1>(1:1, D:1:5,T:1,D:5,F:7,^114m)+   NEW DAR Dec 2010
// RC5x        {36k,msb,889}<1,-1|-1,1>(1:1,~S:1:6,T:1,D:5,-4,S:6,F:6,^114m)+
//
{
	if ( nFrameL < 6 )
		return;
	const float HB = 889; 
	if ( nTotDur < 10.*HB )
		return;
	if ( sortOn.min1 < HB * .3 )
		return;
	if ( sortOn.max1 > HB * 2.4 )
		return;
	if (   !framed(20*HB)  //framed(nGap) is: return nGap <= frame, which pFramedEnd
		|| nMaxDur > HB*6.5     // -4 in RC5x with -1 half bit on either side
        || sortBurst.max2 > HB*4.5  // nominal 2,-2
		|| *pFrame < HB*.5 )
	{
		return;
	}

    // Log of tweaks:
    //  8/14/05 reduced m_maxShortPulse 1.35 -> 1.3 to avoid misdecoding pid_0003 as rc5-1-16
    //
	m_minShortPulse = HB * .3 ;
	m_nominalShortPulse = HB * 1 ;
	m_nominalShortGap = HB * 1 ;
	m_maxShortPulse = HB * 1.3 ;
	m_nominalLongPulse = HB * 2 ;
	m_nominalLongGap = HB * 2 ;
	m_maxLongPulse = HB * 2.4 ;

	m_minShortGap = HB * .5 ;
	m_maxShortGap = HB * 1.4 ;
	m_minLongGap = HB * 1.55 ;
	m_maxLongGap = HB * 2.4 ;

    m_negDelta = 99999.;
    m_posDelta = -99999.;
	/** DAR this also works for RC5
	cleanup();
	if (processManchesterAtoms(0, 13, HB, 2*HB, pFrame) ) {
		*pDevice = getMsb(2,5);
		sprintf(pMisc, "T=%d", getMsb(1,1));
		*pOBC = 64*(getMsb(0,1)^1) + getMsb(7,6);
		strcpy(pProtocol, "RC5");
		return;
	}
	**/
	cleanup();
	nBit = 1;
	nState = 1;

	int bRC5x = 0;
	do
	{
		if (nBit >= sizeof(cBits)*8)
			return;
		int sw = phaseBit2();
		if ( sw <= 0 )
		{
			if ( sw == 0 || nBit != 8 )  
			{
				return;
			}
			double x = *pBit + m_extra;
			if ( nState )
			{
				x = pBit[-1] + pBit[0];
			}
			if ( x < HB * 4.5 || x > HB * 6.5 )
				return;
			++pBit;
			if ( x > HB * 5.5 )
			{
				nState = 1;
				cBits[8>>3] |= 1 << (8&7);
			}
			else
			{
				m_extra = *pBit - HB;
				if ( m_extra < -(HB*.7) || m_extra > (HB*.3) )
					return;
				nState = 0;
				++pBit;
			}
			++nBit;
			bRC5x = 1;
		}
        if ( m_posDelta - m_negDelta > 300. )
            return;
	} while (pBit < pFrameEnd);

	if (nBit < 8)
		return;

	makeMsb();

	*pDevice = cBits[0] & 31;
	// sprintf(pMisc, "T=%d U=%d:%d", (cBits[0]>>5)&1, (int)(m_negDelta+2*HB), (int)(m_posDelta+2*HB));
	sprintf(pMisc, "T=%d", (cBits[0]>>5)&1);	// GD Removed confusing U-values

	*pOBC = (64^cBits[0]&64) + getMsb(nBit-6,6);
	if (bRC5x)
	{
		strcpy(pProtocol, "RC5x");
		if (nBit == 20)
		{
			*pOBC &= 63;
			*pSubDevice = (64^cBits[0]&64) + (cBits[1]>>2);
			return;
		}
	}
	else if ( preemptValue >= prRCx )		// GD 2009  Avoids spurious decode of OrtekMCE
	{
		return;
	}
	else
	{
		strcpy(pProtocol, "RC5");
		if (nBit == 14)
		{
			int nHex = (255-cBits[1])&0xFC;
			pHex[0] = nHex;
			pHex[1] = nHex+1;
			pHex[2] = nHex+2;
			return;
		}
		else if (nBit == 15)
		{
			strcpy(pProtocol, "RC5-7F");
			if ((nFreq > 55000) && (nFreq < 59000)) 
			{
				strcat(pProtocol,"-57");
			}
			*pDevice += 64^cBits[0]&64;
			*pOBC = getMsb(8,7);
			pHex[0] = (255-*pOBC);
			return;
		}
	}

	if (nBit)
	{
		sprintf(pProtocol+strlen(pProtocol), "-%d-%d?", (cBits[0]>>6), nBit);
		if (nBit > 8)
		{
			if (nBit <= 16)
			{
				*pOBC = getMsb(8,nBit-8);
				return;
			}
			*pOBC = getMsb(nBit-8,8);
			*pSubDevice = getMsb(8, nBit-16);
		}
	}
}

bool Signal::processManchesterAtoms(int burstStart, int numBits, float used, float bitDur, float* pFr)   
{
	int i;
	float delta = 0.1 * bitDur;
	float edge;
	float first[2];
	first[0] = 0.5 * bitDur; first[1] = bitDur; 
	int burstIdx = burstStart;
	int burstMax = pFrameEnd - pFr;
	int bitIdx = 0;
	int (*_abs)( int ) = & std::abs;
	do {	
		for (i=0; i<1; i++) { 
			edge = first[i]; 
			if (_abs(pFr[burstIdx]- used - edge) < delta) { 
				cBits[bitIdx >> 3] |= (burstIdx&1) << (7 - bitIdx&7);
				burstIdx++;
				if (_abs(pFr[burstIdx] - (bitDur - edge)) < delta) {
					used = 0.0;
					burstIdx++;
				}
				else
					used = bitDur - edge;
				break;
			}
			else  
				 return false;
		}
		if (i > 1 || burstIdx > burstMax) 
			return false;  //arrive here if no match found
		bitIdx++;
		if (bitIdx >= sizeof(cBits)*8) 
			return false;  //not enough room in cBits
	} while (bitIdx < numBits);
	pBit = pFr + burstIdx;
	nBit = bitIdx;
	return true;
}

void Signal::tryAdNotam()
//
// Ad Notam        {35.7Khz,895,msb}<1,-1|-1,1>(0:1,1:1,D:6,F:6,^114m)+   DAR  August 2012

{
	if ( nFrameL < 6 )
		return;
	const float HB = 889; 
	if ( nTotDur < 10.*HB )
		return;
	if ( sortOn.min1 < HB * .3 )
		return;
	if ( sortOn.max1 > HB * 2.4 )
		return;
	if (   !framed(20*HB)  //framed(nGap) is: return nGap <= frame, which pFramedEnd
		|| nMaxDur > HB*6.5    
        || sortBurst.max2 > HB*4.5  // nominal 2,-2
		|| *pFrame < HB*.5
		|| sortOff.max1 > 3 * HB )
	{
		return;
	}
	cleanup();
    if (!processManchesterAtoms(0, 14, 0, 2*HB, pFrame)) 
		return;
	if (nBit != 14)
		return;
	if (getMsb(0,2) != 1 )
		return;
	*pDevice = getMsb(2,6);
	*pOBC = getMsb(8,6);
	strcpy(pProtocol, "Ad Notam");
}

void Signal::tryRC6()
//
// {36k,444,msb}<-1,1|1,-1>(6,-2,1:1,M:3,<-2,2|2,-2>(T:1),D:8,F:8,^107m)+		RC6 when M=0
// {36k,444,msb}<-1,1|1,-1>(6,-2,1:1,M:3,<-2,2|2,-2>(T:1),D:8,S:8,F:8,^??)+		Replay when M=6
// {36k,444,msb}<-1,1|1,-1>(6,-2,1:1,M:3,-2,2,OEM1:8,OEM2:8,T:1,D:7,F:8,-69m)+   MCE when M=6, OEM1=128
// {36k,444,msb}<-1,1|1,-1>(6,-2,~M:1:3,M:3,<-2,2|2,-2>(T:1),?:?,^??)+          Unknown versions of RC6
//
{
	if ( nFrameL < 10 )
		return;
	const float HB = 444;
	if (   nMaxDur != *pFrame || nMaxDur < 2*HB || nMaxDur > 12*HB	// First ON nominally 6 half bits
		|| *pFrameEnd < 20*HB )
	{
		return;
	}
	cleanup();
	++pBit;	// Skip over nominal 6
    minGlitch = HB*.25;
	do
	{
		switch (nBit)	// Various boundaries within the sequence
		{
		case 0:  // Lead in Bit inverted treated as extra mode bit
			nMinShort = HB;					// Nominally 2
			nMaxShort = nMinLong = HB*2.5;	// 2 vs. 3
			nMaxLong = HB*3.5;				// Nominally 3
			nMinShort2 = HB*.5;				// Nominally 1
			nMaxShort2 = HB*1.5;			// Nominally 1
			break;
		case 1:  // First true mode bit
			nState = 1 - nState;			// Lead bit was processed with reversed polarity
			nMinShort = HB*.5;				// Nominally 1
			nMaxShort = nMinLong = HB*1.5;	// 1 vs. 2
			nMaxLong = HB*2.5;				// Nominally 2
			break;
		case 4:  // Toggle bit
			nBit = 8;						// Put toggle bit in second byte
			nMaxShort = nMinLong = HB*2.;	// 1 vs. 3
			nMaxLong = HB*3.5;				// Nominally 3
			nMinShort2 = HB;				// Nominally 2
			nMaxShort2 = HB*2.5;			// Nominally 2
			break;
		case 9:  // Bit after Toggle bit
			nBit = 16;						// Put in third byte
			nMinShort = HB;					// Nominally 2
			nMaxShort = nMinLong = HB*2.5;	// 2 vs. 3
			nMaxLong = HB*3.5;				// Nominally 3
			nMinShort2 = HB*.5;				// Nominally 1
			nMaxShort2 = HB*1.5;			// Nominally 1
			break;
		case 17:  // Start of normal section
			nMinShort = HB*.5;				// Nominally 1
			nMaxShort = nMinLong = HB*1.5;	// 1 vs. 2
			nMaxLong = HB*2.5;				// Nominally 2
			break;
		case sizeof(cBits)*8:
			return;
		}
		if ( ! phaseBit() )
			return;
	} while (pBit < pFrameEnd);
	if (nBit < 24)
		return;	// Got too little to be right

	makeMsb();
	cBits[0] >>= 4;

	*pOBC = getMsb(nBit-8, 8);

	if (   nBit == 48
		&& cBits[0] == 6
		&& cBits[2] == 128
		&& ! ( cBits[1] & 128 ) )
	{
		sprintf(pMisc, "T=%d", cBits[4]>>7);
		*pDevice = getMsb(33, 7);
		*pSubDevice = getMsb(24, 8);    // OEM2 reported as subdevice
		pHex[0] = *pOBC;
		strcpy(pProtocol, "MCE");
		return;
	}

	sprintf(pMisc, "T=%d", cBits[1]>>7);

	if (nBit > 24)
	{
		*pDevice = cBits[2];
		if (nBit < 32)
			*pDevice = getMsb(16, nBit-16);
	}
	if (nBit == 32 && cBits[0] == 0)
	{
		pHex[0] = *pOBC;
		strcpy(pProtocol, "RC6");
		return;
	}
	if (nBit > 32)
	{
		*pSubDevice = getMsb(24, nBit-32);
	}
	if (nBit == 40 && cBits[0]==6 ) 
	{
		pHex[0] = *pOBC;
		strcpy(pProtocol, "Replay");
		return;
	}
	if ( preemptValue >= prRCx )		// GD 2009  Avoids spurious decode of OrtekMCE
	{
		return;
	}
	sprintf(pProtocol, "RC6-%d-%d",cBits[0], nBit-16);
}

void Signal::tryTDC()	// GD 2009  Added tryTDC

// TDC-38		{38k,315,msb}<-1,1|1,-1>(1:1,D:5,S:5,F:7,-89m)+
// TDC-56		{56.3k,213,msb}<-1,1|1,-1>(1:1,D:5,S:5,F:7,-89m)+
// V2.39 has revised TDC to allow 7-bit OBC as an example has been found of this.
// In 2.38, F:7 was F:6,~F:1

// OrtekMCE		{38.6k,480}<1,-1|-1,1>(4,-1,D:5,P:2,F:6,C:4,-48m)+
// P is position code:	0 for first frame of repeat sequence
//						1 for all repeats up to last one
//						2 for last repeat
// C is checksum, 3 more than the number of 1 bits in D, P, F together

{
	if ( nFrameL < 8 ) return;

	int HB = *pFrame < 260 ? 213 : *pFrame < 400 ? 315 : 480;
	static int FC = 0;		// count of frames bypassed
	int P = 0;				// position count
	static int FP = 0;	// first P value

	if ( nTotDur < 14.*HB )
		return;
	if ( sortOn.min1 < HB * .3 )
		return;
	if ( sortOn.max2 > HB * 2.4 )
		return;
	if (   !framed(40*HB)
		|| nMaxDur > HB*4.5
        || sortBurst.max2 > HB*4.5  // nominal 2,-2
		|| *pFrame < HB*.5 )
	{
		return;
	}

    minGlitch = HB*.25;
	nMinShort = nMinShort2 = HB*.5;				// Nominally 1
	nMaxShort = nMaxShort2 = nMinLong = HB*1.5;	// 1 vs. 2
	nMaxLong = HB*2.5;							// Nominally 2

	cleanup();
	nBit = 1;
	nState = 1;
	++pBit;

	do
	{	
		if ( ! phaseBit() ) return;
	}	while (pBit < pFrameEnd);

	if ( nBit != 18 ) return;		// wrong number of bits

	if ( HB == 480 )
	{	int chksum = -1;	// checksum is checked with equivalent formula on uncomplemented data
		for ( int ndx = 1; ndx < 14; ndx++ )
		{
			chksum += getLsb(ndx,1);
		}

		if ( chksum != getLsb(14,4) ) return;		// wrong checksum

		setPreempt(prByOrtekMCE);					// avoid RC5, RC6 or Q2 decode of same frame
		P = getLsb(6,2);

		// look to see if there is a following frame, usually with different position code P
		float* pFrameEnd1 = pFrameEnd;					// save for later restoration
		float* pFrameEnd2 = pFrameEnd + 1;
		for ( ; pFrameEnd2 < pMainLimit; pFrameEnd2++ )
		{
			if ( *pFrameEnd2 > 40 * HB ) break;
		};
		pBit = pFrameEnd + 2;
		pFrameEnd = pFrameEnd2;							// temp change for use by phaseBit{}
		nBit++;
		nState = 1;

		while ( pBit < pFrameEnd )
		{	
			if ( ! phaseBit() ) break;
		}	
		pFrameEnd = pFrameEnd1;							// restore original value

		if ( nBit == 36 )
		{
			if (	( ( P ==3 || P == 2 ) &&  
						( getLsb(24,2) == 2 || getLsb(24,2) == 1 ) )	// check valid sequence of position count
				&&	getLsb(1,5) == getLsb(19,5)
				&&	getLsb(8,6) == getLsb(26,6)
				&&	getLsb(14,4) - getLsb(32,4) == (getLsb(6,2) == 3 && getLsb(24,2) < 3) )
			{											
				if ( FP == 0 ) FP = P;					// next frame is valid repeat
				if ( FC == 0 ) setzContext();			// save the context value
				FC++;									// increment count of frames skipped
				return;									// wait for repeat to come round
			}
		}

		P = getLsb(6,2);
		*pDevice = 31 - getLsb(1,5);
		*pOBC = 63 - getLsb(8,6);
		pHex[0] = 0xFC - msb(*pOBC);	// need to encode as comp
		nFrameCount = FC;
		strcpy( pProtocol, "OrtekMCE" );
		nNote_out = 6;
//	changed next line to call "strcpy" (was "sprintf"), per instructions
//	by mathdon, for smoother compile in Unix environments--alex750		
		strcpy( pMisc, FP == 3 && P == 1 ? "" :
			FP == 2 && P == 1 ? (nNote_out = 5, nAuxNote_out = 1, "no start frame") : 
			FP == 1 && P == 1 ? (nNote_out = 0, nAuxNote_out = 6, "only end frame") : 		
			FP == 3 && P == 2 ? (nNote_out = 4, nAuxNote_out = 0, "no end frame") : 
			FP == 2 && P == 2 ? (nNote_out = 1, nAuxNote_out = 2, "no start and end frames") :
			(nNote_out = 0, nAuxNote_out = 5, "only start frame") );
	}
	else
	{
		makeMsb();

//		if ( getMsb(16,1) == getMsb(17,1) ) return;	// GD 2.39 protocol revised to allow 7-bit OBC
		*pDevice = getMsb(1,5);
		*pSubDevice = getMsb(6,5);
		*pOBC = getMsb(11,7);						// GD 2.39 In 2.38 was getMsb(11,6)
		pHex[0] = getMsb(11,7) << 1;
		strcpy( pProtocol, (HB == 213) ? "TDC-56" : "TDC-38"  );
	}
	FC = 0;		// reset static variables
	FP = 0;
} // tryTDC

void Signal::tryCanalSat()	// GD 2009  Added tryCanalSat

// CanalSat		{55.5k,250,msb}<-1,1|1,-1>(T=0,(1,-1,D:7,S:6,T:1,0:1,F:7,-89m,T=1)+) 
// where T=0 for the first frame and T=1 for all repeats
// (PID = 018C)
// CanalSatLD     {56k,320,msb}<-1,1|1,-1>(T=0,(1,-1,D:7,S:6,T:1,0:1,F:6,~F:1,-85m,T=1)+)  DAR Aug 2012
// Amino		{56.0k,268,msb}<-1,1|1,-1>(T=1,(7,-6,3,D:4,1:1,T:1,1:2,0:8,F:8,15:4,C:4,-79m,T=0)+)
// also 36k is used
// where T=1 for first frame and T=0 for all repeat frames
// with C = sum of the first 7 nibbles mod 15

// Zaptor		{36k,330,msb}<-1,1|1,-1>(8,-6,2,D:8,T:1,S:7,F:8,E:4,C:4,-74m)+
// where T=0 for all frames except the last, T=1 for last frame, E is a checksum seed
// with C = (D:4+D:4:4+S:4+S:3:4+8*T+F:4+F:4:4+E)&15

{
	int HB;
	static int M = 0;	// toggle mask or previous T	
	int T = 0;				// toggle
	static int FC = 0;	// count of bypassed frames
	static int confBits1to32 = 0;	// DAR July 2011
	static int confBits34to65 = 0;	//filled in when a toggle changes state  DAR July 2011

	if ( nFrameL < 11 || nTotDur < 5000) return;
	if (*pFrame < 1000)
	{	
		if (sortBurst.min3 < 570) 
		{                         
			HB = 250;
		}
		else 
		{
			HB = 320;            //DAR AUG 2012 add CanalSatLD                                       
		}
		if (	*pFrame < HB*.5 
			||	nMaxDur < .5*HB 
			||	nMaxDur > 2.5*HB ) 
		{
			return;
		}
	}
	else
	{
		//if (nFreq > 46000)  //DAR March 2011 Some Amino is 36KHz  
		if (sortBurst.min3 < 600) //so use burst times instead
		{                         
			HB = 270;
		}
		else 
		{
			HB = 330;                                                   
		}
		if (	nMaxDur != *pFrame 
			||	nMaxDur < 3.5*HB 
			||	nMaxDur > 14*HB ) // First ON nominally 7 or 8 half bits
		{
			return;
		}
	}

	if (   !framed(40*HB)
		|| sortOn.min1 < HB * .3
		|| sortOn.max2 > HB * 4.5		// nominal 4
        || sortBurst.max2 > HB * 7  )	// nominal 4,-2
	{
		return;
	}

    minGlitch = HB*.25;
	nMinShort2 = HB * .5;
	nMaxShort2 = HB * 1.5;

	cleanup();
	nBit = 1;
	nState = (HB == 270 || HB == 330) ? 0 : 1;
	pBit += 2 - nState;

	while (pBit < pFrameEnd)
	{
		if ( nBit == 1 || ((HB == 270 || HB == 330) && nBit == 2))
		{
			int pulseL = (HB == 270 || HB == 330) && nBit == 1;						// Is initial long MARK?
			nMinShort = HB * (pulseL ? HB == 330 ? 1.5 : 2.4 : .5);	// nominally 2 : 3 : 1
			nMaxShort = nMinLong = HB * (pulseL ? HB == 330 ? 2.5 : 3.5 : 1.5);	// nominally 2 v 3 : 3 v 4 : 1 v 2
			nMaxLong = HB * (pulseL ? HB == 330 ? 3.6 : 4.8 : 2.5);	// nominally 3 : 4 : 2
		}
		if ( ! phaseBit() ) return;
	}
	if ( nBit != ( (HB == 270 || HB == 330) ? 33 : 23 ) ) return;		// wrong number of bits
	if ( (HB == 250 || HB == 320) && getLsb(15,1) ) return;            // bit 15 must be zero for CanalSat

	// look to see if there is a following frame
	float* pFrameEnd1 = pFrameEnd;					// save for later restoration
	float* pFrameEnd2 = pFrameEnd + 1;
	for ( ; pFrameEnd2 < pMainLimit; pFrameEnd2++ )
	{
		if ( *pFrameEnd2 > 40 * HB ) break;
	};
	nState = (HB == 270 || HB == 330) ? 0 : 1;
	pBit = pFrameEnd + 3 - nState;
	pFrameEnd = pFrameEnd2;							// temp change for use by phaseBit{}
	nBit++;

	while ( pBit < pFrameEnd )
	{
		if ( (HB == 270 || HB == 330) && nBit <= 35 )
		{
			int pulseL = nBit == 34;
			nMinShort = HB * (pulseL ? HB == 330 ? 1.5 : 2.4 : .5);
			nMaxShort = nMinLong = HB * (pulseL ? HB == 330 ? 2.5 : 3.5 : 1.5);
			nMaxLong = HB * (pulseL ? HB == 330 ? 3.6 : 4.8 : 2.5);
		}
		if ( ! phaseBit() ) break;
	}
	pFrameEnd = pFrameEnd1;							// restore original value
	makeMsb();
	if ( (HB == 270 || HB == 330) )  //much of the below Amino/Zaptor stuff revised by DAR July 2011
	{
		int s = 0;	// For Amino andZaptor protocols, check checksum
		int tmp1 = getMsb(1,32);  
		int tmp2 = getMsb(34,32); 
		for (int n=1; n<29; n+=4) s += getMsb(n,4);
		if ( getMsb(29,4) != (s&15) ) return; 
		if ( nBit == 66 )
		{
			s = 0;   // check checksum of second frame
			for (int n=34; n<62; n+=4) s += getMsb(n,4);	
			if ( getMsb(62,4) != (s&15) ) return;
			T = ((tmp1 ^ tmp2) & 0xFFFFFFF0);	//mask off checksum nibble
			if ((T == 0x04000000) 			//Amino confirmed; also means subdevice and function codes match
					&& (tmp1 & 0x04000000))		// repeat bit changed from 1 to 0, so tmp1 is a start frame 
			{
				AminoToggleDetected = true;
				confBits1to32 = tmp1;
				confBits34to65 = tmp2;
				if ( FC == 0 ) setzContext();			// save the context value
				FC++;	
				return;			// wait for repeat to come round
			}
			if	((T == 0x00800000)			//Zaptor confirmed; also shows subdevice and function codes match
					&& (tmp2 & 0x00800000)) //repeat bit changed from 0 to 1, so tmp2 is an end frame 
			{  
				ZaptorToggleDetected = true;
				confBits1to32 = tmp1;
				confBits34to65 = tmp2;
			}
		}
	}
	if ( (HB == 250 || HB == 320)&&	nBit == 46 )
	{
		T = getMsb(14,1) ^ getMsb(37,1);				// difference between the toggle values
		if	(	( (M == 0) ^ (T == 0) )					// exactly one of M and T must be zero
			&&	getMsb(1,13) == getMsb(24,13)			// all bits other than toggle must be equal
			&&	getMsb(15,8) == getMsb(38,8) )
		{
			M = 1;										// next frame is valid repeat
			if ( FC == 0 ) setzContext();				// save the context value
			FC++;										// increment count of bypassed frames
			return;										// wait for repeat to come round
		}
	}					
	if ( HB == 250 )
	{
		*pDevice = getMsb(1,7);
		*pSubDevice = getMsb(8,6);
		*pOBC = getMsb(16,7);
		pHex[0] = *pOBC << 1;
		strcpy( pProtocol, "CanalSat" );
		nNote_out = 4;
		strcpy( pMisc, M == 0 ? (nNote_out = 1, nAuxNote_out = 1, "no start frame") : "");
	}
	else if ( HB == 320 )
	{
		*pDevice = getMsb(1,7);
		*pSubDevice = getMsb(8,6);
		*pOBC = getMsb(16,6);
		strcpy( pProtocol, "CanalSatLD" );
		nNote_out = 4;
		strcpy( pMisc, M == 0 ? (nNote_out = 1, nAuxNote_out = 1, "no start frame") : "");
	}
	else 
	{
		if (AminoToggleDetected) HB = 270;		
		else if (ZaptorToggleDetected) HB = 330;	
		else 
		{
			confBits1to32 = getMsb(1,32);
			int tmp3 = confBits1to32 & 0x000000F0; 
			if (tmp3 == 0xF0) HB = 270;			//Amino decided by seed nibble
			else if (tmp3 == 0x50) HB = 330;	//Zaptor 
		}	//  else decide protocol by already determined HB duration 	
		if ( HB == 270 )					//Amino
		{
			*pDevice = (confBits1to32 >> 28) & 0xF;	
			*pSubDevice = (confBits1to32 >> 16) & 0xFF;  //always zero according to the Amino spec
			*pOBC = (confBits1to32 >> 8) & 0xFF;   
			pHex[0] = *pOBC;
			int seed = (confBits1to32 >> 4) & 0xF;
			if (seed != 0x0F) sprintf(pMisc, "E = %d", seed);
			if (nFreq > 46000) strcpy( pProtocol, "Amino-56");
			else strcpy( pProtocol, "Amino-36");
			if (AminoToggleDetected)  nNote_out = 4;
			else
			{
				nNote_out = 1;
				if ((confBits1to32 & 0x04000000) == 0) 
				{
					nAuxNote_out = 1;
					if (*pMisc){strcat(pMisc, ", ");}
					strcat(pMisc, "no start frame");
				}
				else 
				{
					nAuxNote_out = 0;
					if (*pMisc){strcat(pMisc, ", ");}
					strcat(pMisc, "no end frame");
				}
			}
		}
		else if ( HB == 330 )  //Zaptor
		{
			if (!ZaptorToggleDetected && (nBit == 66))  
			{
					if ( FC == 0 ) setzContext();	// save the context value
					FC++;	
					return;			// wait for repeat to come round
			}
			*pDevice = (confBits1to32 >> 24) & 0xFF;
			*pSubDevice = (confBits1to32 >> 16) & 0x7F;  
			*pOBC = (confBits1to32 >> 8) & 0xFF;   
			pHex[0] = *pOBC;
			int seed = (confBits1to32 >> 4) & 0xF;
			if (nFreq > 46000) strcpy( pProtocol, "Zaptor-56");
			else strcpy( pProtocol, "Zaptor-36");
			if (seed != 5) sprintf(pMisc, "E = %d", seed);
			if (ZaptorToggleDetected) nNote_out = 5;  
			else if ((confBits1to32 & 0x00800000) == 0)
			{
				nNote_out = 1;
				nAuxNote_out = 0;
				if (*pMisc){strcat(pMisc, ", ");}
				strcat(pMisc, "no end frame");
			}
			else
			{
				nNote_out = 0;
				nAuxNote_out = 6;
				if (*pMisc){strcat(pMisc, ", ");}
				strcat(pMisc, "no start frame");
			}
		}
	}
	nFrameCount = FC;
	M = 0;		// reset static variables
	FC = 0;		
} // tryCanalSat

void Signal::trySejin()  // GD 2009  Added trySejin
// PID = 0161 (0161:3 for Sejin-1, 0161:5 for both Sejin-1 and Sejin-2 in one upgrade.)
//
// The Sejin protocols have two signal styles, Sejin-1 and Sejin-2, and two frequency 
// variants, 38 and 56.  The signal styles have the following common structure:
//
// Sejin-M-38	{38.8k,310,msb}<-1|1>(<8:4|4:4|2:4|1:4>(3,3:2,Dx:8,Fx:8,Fy:8,E:4,C:4,-L))+
// Sejin-M-56	{56.3k,310,msb}<-1|1>(<8:4|4:4|2:4|1:4>(3,3:2,Dx:8,Fx:8,Fy:8,E:4,C:4,-L))+
//
// where E is a checksum seed (0 in all known examples) and C is the checksum given by
// C = (Dx:4 + Dx:4:4 + Fx:4 + Fx:4:4 + Fy:4 + Fy:4:4 + E) & 15.
//
// Dx is a signed 8-bit integer, ie range -128 to +127. If Dx >= 0 the style is Sejin-1 and 
// the leadout time L is 77m.  This style is used for normal remote-control buttons.  If
// Dx < 0 the style is Sejin-2 and the leadout time L is the much shorter 3.6m.  This style is
// used for signals corresponding to a pointing device with up to 3 buttons.  The short
// leadout allows for much faster repeat action.  The signal parameters Dx, Fx, Fy have 
// different significance in the two styles.
//
// The data parameters in Sejin-1 are 7-bit device and subdevice codes D and S, an 8-bit
// function code F and a toggle bit T.  The signal repeats as long as the key is held, with
// T=0 for all frames except the last, T=1 for the last frame.  The corresponding signal
// parameters are:
//             Dx = D, Fx:1:7 = T, Fx:7 = S, Fy = F.
//
// Sejin-2 has a 5-bit device code D and no subdevice code.  It further divides into 
// two sub-styles, 2A corresponding to movement of the pointing device, and
// 2B corresponding to movement of its buttons.  In Sejin-2A the data parameters are the
// coordinates (X,Y) of the displacement of a cursor and the signal parameters are
//             Dx:6:2 = (-D):6,  Dx:2 = 0, Fx = X:8, Fy = Y:8.
// Note that X and Y can be negative, so Fx and Fy are signed 8-bit integers.  The signal
// repeats indefinitely.
//
// Sejin-2B corresponds to down and up movements of the buttons of the pointing device.
// A "down" signal is sent exactly once on button down (distinct for each button) and an
// "up" signal is sent exactly once on button up (same for each button).  There is one
// data parameter, B.  When B = 1,2 or 3 it denotes button B down, when B = 0 it denotes
// button up.  The signal parameters are
//             Dx:6:2 = (-D):6,  Dx:2 = B:2, Fx = 0, Fy = 0.
//
// The 0161:5 protocol can only generate signals in which one of Fx or Fy is zero.  It
// uses (-D)*4 as a device byte and has two function bytes.  The first is the nonzero
// one of Fx and Fy (or 0 if both are 0).  The second has B in bits 0 and 1 (0 for
// Sejin-2A), bit 4 is set for Sejin-2A and clear for Sejin-2B and bit 3 flags which of
// X or Y is nonzero in Sejin-2A (set for X, clear for Y).
//
// The Misc field includes a 13-byte integer RMOBC which is these two bytes in decimal
// form.  It can be entered in RemoteMaster into the RMOBC column of the function page
// for the Sejin-M-N protocol.

{
	if ( nFrameL < 15 || nTotDur < 18000 || nTotDur > 26000) return;

	const float unit = 310.;
	static int FC = 0;					// count of bypassed frames

	if (	!framed(10 * unit)			// must allow for 3.6ms leadout for Sejin type 2
		||	sortOn.max2 > 2.4 * unit
		||	sortOn.min1 < 0.5 * unit )
	{
		return;
	}

	// Next condition added in v2.40 as a specific JVC{2} signal was giving spurious Sejin decode.
	if (	pFrame[0] < 700 || pFrame[0] > 1150		// nominal 930, allow 25% either way
		||	pFrame[1] < 700 || pFrame[1] > 1150 )	// nominal 930, allow 25% either way
	{
		return;
	}

	cleanup();
	pBit++;			// skip initial 3-unit ON
	int bcnt = 0;	// bit count (outer, <-1|1>, coding) to start of next OFF
	int dbit;		// double bit (inner coding)
	while (pBit < pFrameEnd)
	{
		bcnt += (int)( (pBit[0] + pBit[1]) / unit + 0.5 );	// advance to start of following OFF
		pBit += 2;
		while (bcnt > 2 * nBit)
		{
			dbit = min( bcnt - 2 * nBit - 1, 3 );		// make next double bit
			cBits[nBit>>3] |= dbit << (6 - (nBit & 6));	// add to cBits in msb form
			nBit += 2;
		}
	}
	if (	nBit != 34											// number of bits
		||	getMsb(0,2) != 3									// initial double bit
		||	getMsb(30,4) != ( getMsb(2,4) + getMsb(6,4) + getMsb(10,4) + 
				getMsb(14,4) + getMsb(18,4) + getMsb(22,4) + getMsb(26,4) ) % 16	// checksum
		||	!getMsb(2,1) && !framed(100*unit) )	// long leadout for Sejin type 1
	{
		return;
	}

	// look to see if there is a following frame
	float* pFrameEnd2 = pFrameEnd + 1;
	for ( ; pFrameEnd2 < pMainLimit; pFrameEnd2++ )
	{
		if ( *pFrameEnd2 > 10 * unit ) break;
	};
	pBit += 2;
	bcnt = 2 * nBit;
	while (pBit < pFrameEnd2)
	{
		bcnt += (int)( (pBit[0] + pBit[1]) / unit + 0.5 );	// advance to start of following OFF
		pBit += 2;
		while (bcnt > 2 * nBit)
		{
			dbit = min( bcnt - 2 * nBit - 1, 3 );		// make next double bit
			cBits[nBit>>3] |= dbit << (6 - (nBit & 6));	// add to cBits in msb form
			nBit += 2;
		}
	}

	if (	nBit == 68
		&&	getMsb(0,10) == getMsb(34,10)
		&&	(getMsb(2,1) || getMsb(10,1) == 0)		// if type 1 then T=0 except for last frame
		&&	(!getMsb(2,1) || getMsb(10,1) == getMsb(44,1)) // if type 2 then no toggle
		&&	getMsb(11,19) == getMsb(45,19)
		&&	getMsb(31,3) == getMsb(65,3)
		&&  ( getMsb(30,1)^getMsb(64,1) ) == ( getMsb(10,1)^getMsb(44,1) )  // chksum affected by toggle
		&&  (getMsb(36,1) || (*pFrameEnd2 >= 100 * unit) ) )  // either type 2 or long leadout
	{
		if ( FC == 0 ) setzContext();			// save the context value
		FC++;	// increment count of bypassed frames
		return;	// another frame follows, so wait for it to come round
	}

	if ( !getMsb(2,1) )		// type 1
	{
		*pDevice = getMsb(2,8);
		*pSubDevice = getMsb(11,7);
		*pOBC = getMsb(18,8);
		pHex[0] = *pOBC;
		strcpy(pProtocol, nFreq < 45000 ? "Sejin-1-38" : "Sejin-1-56");
		sprintf(pMisc, getMsb(10,1) ? "E=%d" : "E=%d, no end frame", getMsb(26,4));
	}
	else				// type 2
	{	
		// RMOBC is the decimal form of the 2-byte hex function code needed in RemoteMaster
		// with protocol 0161:5 to generate the signal concerned

		int X = getMsb(10,1) ? getMsb(10,8)-256 : getMsb(10,8); // X coord of delta
		int Y = getMsb(18,1) ? getMsb(18,8)-256 : getMsb(18,8); // Y coord of delta
		int Fn1 = getMsb(10,8) ? getMsb(10,8) : getMsb(18,8);	// 1st function byte
		int Fn2 = getMsb(8,2);									// 2nd function byte
		*pDevice = (64-getMsb(2,6));
		*pOBC = Fn2;
		Fn2 += getMsb(10,8) ? 0x08 : 0;		// flag set if X coord, clear if Y coord
		Fn2 += Fn1 ? 0x10 : 0;				// flag set if delta, clear if button down/up 
		strcpy(pProtocol, nFreq < 45000 ? "Sejin-2-38" : "Sejin-2-56");
		if ( X == 0 && Y == 0 && *pOBC == 0 )
		{
			sprintf(pMisc, "Btn up; E=%d, RMOBC=%d", getMsb(26,4), Fn1 + 0x100*Fn2);
		}
		else if ( X == 0 && Y == 0 )
		{
			sprintf(pMisc, "Btn down (OBC=Btn nbr); E=%d, RMOBC=%d", getMsb(26,4), Fn1 + 0x100*Fn2);
		}
		else if ( X == 0 || Y == 0 )
		{
			sprintf(pMisc, "delta = (%d,%d); E=%d, RMOBC=%d", X, Y, getMsb(26,4), Fn1 + 0x100*Fn2);
		}
		else
		{
			sprintf(pMisc, "delta = (%d,%d); E=%d, RMOBC unsupported", X, Y, getMsb(26,4) );
		}
	}
	nFrameCount = FC;
	FC = 0;		// reset static variable
}  // trySejin

void Signal::tryNokia()
// Nokia12 {36k,msb}<164,-276|164,-445|164,-614|164,-783>(412,-276,D:4,F:8,164,-???)+
// Nokia   {36k,msb}<164,-276|164,-445|164,-614|164,-783>(412,-276,D:8,S:8,F:8,164,-???)+
// Nokia32 {36k,msb}<164,-276|164,-445|164,-614|164,-783>(412,-276,D:8,S:8,X:8,F:8,164,^100m)+
{
	enum { ON=164, BASE=276, UNIT=169 };
	const double SCALE = 1./UNIT;

	if (   (nFrameL != 8 && nFrameL != 14 && nFrameL != 18)
		|| *pFrameEnd <= BASE+UNIT*4.5 )  // Require some end framing
	{
		return;
	}
	int (*_abs)( int ) = & std::abs;
    if (   pFrame[0] <= sortOn.max2           // Require that the first On be largest
		|| sortOn.max2 > ON*1.5               // Require that the largest On other than the first not be excessive
		|| sortBurst.max1 > BASE+UNIT*4.5     // Require that the largest Burst not be excessive
		|| pFrame[0]+pFrame[1] > BASE+UNIT*3. // Require first pair not too large
		||_abs(sortOff.min1/sortOn.min1 - 276./164.) > 0.2  ) //require about the correct ratio of durations DAR 2012
	{
		return;
	}
//	if (   nFrameL == 8
//		&& pOff[0] + UNIT*.5 > pOff[5] )
//	{
//		return;
//	}
	cleanup();

	for (int ndx=0; ndx<nFrameL-2; ndx++)
	{
		int nVal = (int)floor((pFrame[ndx*2+3] - (BASE-UNIT*.5) ) * SCALE);
		if (nVal<0 || nVal>3)
			return;

		cBits[ndx>>2] |= nVal << (6-(ndx&3)*2);
	}
	if ( nFrameL == 8 )
	{
		strcpy(pProtocol, "Nokia12");
		*pDevice = getMsb(0,4);		// Was getLsb in both places, bug found by Barf
		*pOBC = getMsb(4,8);
	}
	else
	{
		*pDevice = cBits[0];
		*pSubDevice = cBits[1];
		if ( nFrameL == 14 )
		{
			strcpy(pProtocol, "Nokia");
			*pOBC = cBits[2];
		}
		else
		{
			strcpy(pProtocol, "Nokia32");
			*pOBC = cBits[3];
			sprintf(pMisc,"X=%d T=%d", cBits[2]&0x7F, (cBits[2]&0x80) >> 7 );
		}
	}
	pHex[0] = *pOBC;
}

void Signal::tryF12()   //  K=(D:3,H:1,F:8) {37.9k,422}<1,-3|3,-1>(K,-80,K) for H=0. 
						//{37.9k,422}<1,-3|3,-1>(K,-80,K,-208,K,-80,K)+ for H=1. 
		//F32:			//{38k,420, msb}<1,-3|3,-1>(D:8,S:8,F:8,E:8,-100m)* 
{
	if ( nFrameL != 12 && nFrameL != 32 || !framed(nMaxDur*16) )
		return;
    if ( sortBurst.min1 * 1.15 < sortBurst.max1 )
        return;
    nMaxShort = sortBurst.min1 * .4;
    cleanup();
	for (int ndx=0; ndx<nFrameL; ++ndx)
	{
		if (pFrame[2*ndx] > nMaxShort)
		{
			if (pFrame[2*ndx+1] > nMaxShort && ndx<nFrameL-1)
				return;
			cBits[ndx>>3] |= 1 << (ndx&7);
		}
	}
	if (nFrameL == 12) {
		strcpy(pProtocol,"F12");
		*pDevice = getLsb(0,3);
		*pSubDevice = getLsb(3,1);
		*pOBC = getLsb(4,8);
	}
	else {
		strcpy(pProtocol,"F32");
		*pDevice = msb(cBits[0]);
		*pSubDevice =  msb(cBits[1]);
		*pOBC =  msb(cBits[2]);
		sprintf(pMisc, "E = %d",  msb(cBits[3]));
	}
}  // tryF12

void Signal::tryElan()   // {40.2k,398,msb}<1,-1|1,-2>(3,-2,D:8,~D:8,2,-2,F:8,~F:8,1,^50m)+  DAR 2012
{
	if ( nFrameL != 35)
		return;
    if ( sortOff.min1 * 3.6 < sortBurst.max3  || sortOff.min1 * 2.4 > sortBurst.max3)
        return;
	if (pFrame[34] > 2.4 * sortOff.min1 || pFrame[34] < 1.6 * sortOff.min1 
		|| pFrame[35] > 2.4 * sortOff.min1 || pFrame[35] < 1.6 * sortOff.min1 )
		return;
	cleanup();
	nMaxShort = sortBurst.min1 * .4;
	int cNdx = 0;
	for (int ndx=1; ndx<35; ++ndx)
	{
		if (ndx == 17) ndx++;
		if (pFrame[2*ndx] > nMaxShort)
		{
			if (pFrame[2*ndx+1] > 2 * nMaxShort)
				cBits[cNdx>>3] |= 0x80 >> (cNdx&7);
		}
		cNdx++;
	}
	int dev = cBits[0];
	int devComp = cBits[1];
	int cmd = cBits[2];
	int cmdComp = cBits[3];
	if ((0xFF ^ devComp) !=  dev || (0xFF ^ cmdComp) != cmd)
		return;
	strcpy(pProtocol,"Elan");
	*pDevice = dev;
	*pOBC = cmd;
}  // tryElan


void Signal::tryBryston()   //  {38.0k,315} <1,-6|6,-1>(D:10, F:8, -18)  //DAR 2012 
{
	if ( nFrameL != 18 || !framed(nMaxDur*16) )
		return;
    if ( sortBurst.min1 * 1.15 < sortBurst.max1 )
        return;
	 if ( sortOff.min1 * 8 < sortBurst.max3  || sortOff.min1 * 6 > sortBurst.max3)
        return;
    nMaxShort = sortBurst.min1 * .4;
    cleanup();
	for (int ndx=0; ndx<nFrameL; ++ndx)
	{
		if (pFrame[2*ndx] > nMaxShort)
		{
			if (pFrame[2*ndx+1] > nMaxShort && ndx<nFrameL-1)
				return;
			cBits[ndx>>3] |= 1 << (ndx&7);
		}
	}
	strcpy(pProtocol,"Bryston");
	*pDevice = getLsb(0,10);
	*pOBC = getLsb(10,8);
}  // tryBryston


bool Signal::processHumaxAtoms(int bitStart, float* pFr, int maxBursts)   
{
	int atomIdx,i;
	float one = 105.0;
	float four = 4* one;
	float delta = one * 0.5;
	float edge, used = 0.0;
	float first[4];
	first[0] = 2*one; first[1] = 3*one; first[2] = one; first[3] = 2*one;
	int burstIdx = 0;
	int bitIdx=bitStart;
	int (*_abs)( int ) = & std::abs;
	do {	
		atomIdx=2 * (1 - burstIdx&1); //odd burstIdx is "off" signal, which can only be the beginning of bits value 0 or 1
		for (i=0; i<2; i++) { //<-2,2|-3,1|1,-3|2,-2>
			edge = first[atomIdx + i ]; 
			if (_abs(pFr[burstIdx]- used - edge) < delta) {
				cBits[bitIdx >> 2] |= (atomIdx + i) << (6- 2*(bitIdx&3));
				burstIdx++;
				if (_abs(pFr[burstIdx] - (four - edge)) < delta) {
					used = 0.0;
					burstIdx++;
				}
				else
					used = four - edge;
				break;
			}
		}
		if (i > 1) 
			return false;  //arrive here if no match found
		bitIdx++;
		if (bitIdx >= sizeof(cBits)*4) 
			return false;  //not enough room in cBits
		if (bitIdx % 12 == 0) {
			used = 0.0;
			burstIdx++;
		}
	} while (burstIdx < maxBursts );
	if (bitIdx % 12 != 0)
		return false;  // wrong frame length
	int cIdx = (bitIdx - 1) >> 2;
	if ((cBits[cIdx] & 2) == (cBits[cIdx] & 1)) 
		return false;  // failed check bit
	return true;
}

void Signal::tryHumax()   //  {56k,105, msb}<-2,2|-3,1|1,-3|2,-2>(T=0,(2,-2,D:7, S:5,T:2,F:6:1,(F:1+1):2),^95m,T=1)+) DAR 2012 
{
	static int M = 0;	// toggle mask or previous T	
	int T;
	static int FC = 0;	// count of bypassed frames
	bool success;
	if (( nFrameL < 7 ) || !framed(420*12) )
		return;
	if (sortOff.max1 > 1.5 * 630) 
		return;
    //if ( sortOn.min1 * 3.6 < sortOff.min1 )
        //return;
	 //if ( sortOff.min1 * 2.5 < sortBurst.min1  || sortOff.min1 * 1.8 > sortBurst.max3)
        //return;
	cleanup();
	success = processHumaxAtoms(0, pFrame, pFrameEnd - pFrame);
	if (!success)
		return;
	float* pFrameEnd2 = pFrameEnd + 1;
	for ( ; pFrameEnd2 < pMainLimit; pFrameEnd2++ ) {
		if ( *pFrameEnd2 > 2000. ) break; // about 3 time the max duration
	};
	success = processHumaxAtoms(12, pFrameEnd + 1, pFrameEnd2 - pFrameEnd); //check for second signal
	setPreempt(prAsync	);  // avoid an Async decode
	makeMsb();
	if (success)  {
		T = getMsb(14,2) ^ getMsb(38,2);				// difference between the toggle values
		if	(( (M == 0) ^ (T == 0) )					// exactly one of M and T must be zero
			&&	getMsb(0,13) == getMsb(24,13)			// all bits other than toggle must be equal
			&&	getMsb(16,8) == getMsb(40,8) )
		{
			M = 1;										// next frame is valid repeat
			if ( FC == 0 ) setzContext();				// save the context value
			FC++;										// increment count of bypassed frames
			return;										// wait for repeat to come round
		}
	}
	strcpy(pProtocol,"Humax 4Phase"); 
    *pDevice = getMsb(2,6);
	*pSubDevice = getMsb(8,6);
	int tog = getMsb(14,2);
	sprintf(pMisc, "T=%d", tog);
	*pOBC = getMsb(16,7);
	nNote_out = 4;
	strcpy( pMisc, M == 0 ? (nNote_out = 1, nAuxNote_out = 1, "no start frame") : "");
	nFrameCount = FC;
	FC = 0;		// reset static variables
	M = 0;	
}  // tryHumax

void Signal::tryX10()
{
	// <2,-13|7,-7>(8,-8, F:5, ~F:5, 23,-8)+

	// GD 2009: That IRP is only the second part of the protocol that is decoded here.
	// A more complete description is
	// <2,-13|7,-7>(8,-8,F:5,N:-4,23,-8,(8,-8, F:5, ~F:5, 23,-8)+)
	// where N is a 4-bit counter (incremented at each keypress, though that doesn't
	// show in the decode).  However, according to the UEI executor for
	// the X10 protocol, PID 003F, the start frame should not have the (8,-8) 
	// leadin, so that frame is not picked up.  In addition, the UEI timing
	// is slightly different, which may be for convenience but it means that
	// the repeat frame of the UEI executor is also often not picked up.  UEI uses
	//
	// {40.8k,565}<2,-12|7,-7>(F:5,N:-4,21,-7,(7,-7,F:5,~F:5,21,-7)+).
	//
	// This has all bursts of equal length while the IRP above has the lead-in burst
	// being the longest.  The decode tests whether the lead-in is the longest
	// burst and random variations in the UEI execution mean that is often is not, so
	// the frame is missed.  I have removed this test, corrected the start frame and
	// added a test on burst lengths that allows that the (8,-8) lead-in may be correct.

//	if ( nFrameL > 12 || nFrameL < 11 )
	if ( nFrameL != 12 && nFrameL != 10 )		// GD 2009
		return;
	if ( sortOn.max1 <= sortBurst.max1 )	// leadOutOn should exceed sortBurst.max1
		return;
	/*  GD 2009 Allow for all bursts to have same nominal length, so next condition
		removed and following one modified
	if ( pFrame[0]+pFrame[1] <= sortBurst.max2 )
		return;
	*/
	if ( sortBurst.min1*5 <= /* sortBurst.max2*4 */ sortBurst.max1*4 )
		return;
	// GD 2009 Test burst lengths
	if ( sortBurst.min1 < 0.8*14*565 || sortBurst.max1 > 1.2*16*565 )
		return;
	if ( !framedLeft(sortOn.max1) )
		if ( pFrame[-2] < sortBurst.max1 )
			if (   pFrame != pDuration+2*nSingle
				|| pDuration[2*(nSingle+nRepeat)-2] < sortBurst.max1 )
					return;
	cleanup();
	nMaxShort = sortBurst.min1/3;
	decodeX(11);
	int obc = /* getLsb(1,5); */ getLsb(0,6);	// GD 2009 Covers both frame situations
	if ( nFrameL == 12 )
	{
		if ( (obc & 1) != 1 )					// GD 2009 Test lead-in bit 
			return;
		pHex[0] = msb(obc);					// GD 2009 Lead-in 1-bit is included in hex
		obc >>= 1;							// GD 2009 Remove lead-in bit
		if ( getLsb(6,5) + obc != 31 )
			return;
		strcpy(pProtocol,"X10");
		*pOBC = obc;
		bInitLeadIn = false;				// GD 2009
		return;
	}
	sprintf(pProtocol,"X10:%d", msb( getLsb(/* 6 */ 5,4), 4) );
	obc &= 0x1F;							// GD 2009 Remove spurious 6th bit
	*pOBC = obc;	
	pHex[0] = msb(obc<<1 | 1);				// GD 2009 Lead-in 1-bit must be included in hex
	nNote_out = 0;
	nAuxNote_out = 5;
	strcpy(pMisc, "invalid signal");
	bInitLeadIn = true;						// GD 2009
}  // tryX10

// (38k,560,msb)<1,-1|3,-1>(16,-8, D:4,F:8,~D:4,~F:8 -32)+
void Signal::trySunfire()
{
	if (nFrameL != 25)
		return;
	if ( pFrame[1] <= sortBurst.max2 )
		return;
	if ( ! framed(sortBurst.max1) )
		return;
	cleanup();
   	pBit = pFrame+2;
	nMaxShort = sortBurst.min1;
	decodeX(24);
	if (getLsb(0,12) + getLsb(12,12) != 0xFFF )
		return;
	makeMsb();
	strcpy(pProtocol, "Sunfire");
	*pDevice = getMsb(0,4);
	*pOBC = getMsb(4,8);

}  // trySunfire

// Grundig16 (PID 0112)
// Grundig16-30 (PID 00AB)
// GD: IRP corrected by GD after discussion with John Fine.
// Grundig16 {35.7k,578,msb}<-4,2|-3,1,-1,1|-2,1,-2,1|-1,1,-3,1>(806u,-2960u,1346u,T:1,F:8,D:7,-100)+
// Grundig16-30 {30.3k,578,msb}<-4,2|-3,1,-1,1|-2,1,-2,1|-1,1,-3,1>(806u,-2960u,1346u,T:1,F:8,D:7,-100)+
void Signal::tryGrundig16()
{
    if ( pFrameEnd[0] < 5000 )
        return;
	if (nTotDur < 29800 || nTotDur > 35800)
		return;
	if (nFrameL < 10 || nFrameL > 18)
		return;
	float* pB = pFrame+3;
	float nMin=99999;
	float nMax=0;
	cleanup();		// GD 2009   added for safety
	for (nBit=0; nBit<16; nBit+=2)
	{
		float tot = pB[0] + pB[1];
		unsigned char& cB = cBits[nBit>>3];
		unsigned int cN = (nBit & 6);
		switch ( (int)((*pB+144)*(1./289.)) )
		{
		case 2:
			cB |= (0xC0 >> cN);
			pB += 2;
			tot += pB[0] + pB[1];
			break;
		case 4:
			cB = (cB & (0x3F3F >> cN)) + (0x80 >> cN);
			pB += 2;
			tot += pB[0] + pB[1];
			break;
		case 6:
			cB = (cB & (0x3F3F >> cN)) + (0x40 >> cN);
			pB += 2;
			tot += pB[0] + pB[1];
			break;
		case 8:
			cB &= (0x3F3F >> cN);
			break;
		default:
			return;
		}
		pB+=2;
		if (pB > pFrameEnd)
			return;
		if (tot < 3000 || tot > 4161)
			return;
		if (nMin > tot)
			nMin = tot;
		if (nMax < tot)
			nMax = tot;
	}
	if (pB != pFrameEnd)
		return;
	strcpy(pProtocol, "Grundig16");
	if (nFreq < 33000) strcat(pProtocol, "-30");
	
	*pDevice = getMsb(9,7);
	*pOBC = getMsb(1,8);
	int hx = getMsb(2,8);
	*pHex = ((hx & 0x55)<<1) + (((hx>>1)^hx) & 0x55);
//	sprintf(pMisc, "T=%d tot=%.0f U=%.0f_%.0f", getMsb(0,1), nTotDur, nMin, nMax);
	sprintf(pMisc, "T=%d", getMsb(0,1));
}  // tryGrundig16

// {35.7k}<308,-881|669,-520>(2072,-484,F:2,D:3,C:4,-2300)+
// C = F*4 + D + 3
// '1' usually mislearns as 308,-90,272,-520
void Signal::trySomfy()
{
    if (   nFrameL < 10
        || nFrameL > 19 )
    {
        return;
    }
    float firstBurst = pFrame[0] + pFrame[1];
    if (   firstBurst < 1.6 * sortBurst.max2
        || sortBurst.max1 > pFrameEnd[0] )
    {
        return;
    }
    float bitTotal = nTotDur - firstBurst;
    if ( bitTotal > 8.7 * sortBurst.max2 )
        return;
    if ( bitTotal < 7. * sortBurst.max2 )
        return;
    cBits[0] = cBits[1] = 0;
	pBit = pFrame+2;
	nBit = 0;
    for (;;)
    {
        float burst = pBit[0] + pBit[1];
        if ( burst * 1.6 > sortBurst.max2 )
        {
            if ( pBit+1 >= pFrameEnd )
            {
                if ( nBit != 8 )
                    return;
                if ( pBit[0] * 2.1 > sortBurst.max2 )
    			    cBits[nBit>>3] |= 1 << (nBit & 7);
                break;
            }
            if ( pBit[0] > pBit[1] )
    			cBits[nBit>>3] |= 1 << (nBit & 7);
            pBit += 2;
        }
        else
        {
            pBit += 2;
            burst += pBit[0];
            if ( pBit+1 >= pFrameEnd )
            {
                if ( nBit != 8 )
                    return;
                if ( burst *2.1 < sortBurst.max2 )
                    return;
                if ( burst > sortBurst.max2 )
                    return;
  			    cBits[nBit>>3] |= 1 << (nBit & 7);
                break;
            }
            burst += pBit[1];
            if (   burst *1.3 < sortBurst.max2
                || burst *.8 > sortBurst.max2
                || burst *.5 < pBit[1] )
            {
                return;
            }
    		cBits[nBit>>3] |= 1 << (nBit & 7);
            pBit += 2;
        }
        if ( ++nBit == 9 )
        {
            return;
        }
    }
    int cmd = getLsb(0,2);
    int dev = getLsb(2,3);
    if ( ( (cmd*4+dev+3) & 15 ) == getLsb(5,4) )
    {
	    strcpy(pProtocol, "Somfy");
	    *pOBC = cmd;
        *pDevice = dev;
    }

} // trySomfy

// {38k,msb}<210 -760|210 -896|210 -1032|210 -1168|210 -1304|210 -1440|210 -1576|210 -1712|210 -1848|210 -1984|210 -2120|210 -2256|210 -2392|210 -2528|210 -2664|210 -2800>
// (A:4,C:4,S:4,15:4,B:8,D:8,210,-13.8m,A:4,X:4,G:4,J:4,F:16,210,-80.4m,(A:4,C:4,S:4,15:4,B:8,D:8,210,-13.8m,A:4,Y:4,H:4,J:4,F:16,210,-80.4m)+)
// C, X, Y are check nibbles each making the group of 8 nibbles containing them add to zero mod 16
// B defaults to 0x44, if any other value then it is reported as OEM code.
// G = 0
// H = 8
// J = S
// Subdevice reported as A*16 + S

// Velodyne: {38k,136,msb}<210,-760>(<0:1|0:1,-1|0:1,-2|0:1,-3|0:1,-4|0:1,-5|0:1,-6|0:1,-7|0:1,-8|0:1,-9|0:1,-10|0:1,-11|0:1,-12|0:1,-13|0:1,-14|0:1,-15>
//           (S:4:4,C1:4,S:4,15:4,D:4,0:4,F:8,210u,-79m,S:4:4,C2:4,S:4,15:4,D:4,8:4,F:8,210u,-79m)+)
//           {C1=-(8+S+S::4+15+D+0+F+F::4),C2=-(8+S+S::4+15+D+8+F+F::4)  //DAR July 2011

void Signal::tryXMP()
{
	// Although the true frame length must be 18, we allow for up to two missing bursts.  If one
	// of these is the lead-out then the frame will appear to end after the next half-frame, giving
	// lengths of 25 or 26.  If there is a final frame with a toggle nibble of 9 then the lead-out
	// preceding it can be the same as the intermediate lead-out 13.8ms and always seems smaller
	// than the full 80.4ms.  So at this point we only test for intermediate-size lead-outs.
	if (   	!(nFrameL >= 16 && nFrameL <= 18 || nFrameL >= 25 && nFrameL <= 26) 
		||	!(framed(sortBurst.max2 * 3.) && framed(8000.) ) )
	{
		return;
	}

	// Test the separating gap between two half-frames.
	if (	pFrame[17] < sortBurst.max2 * 3. 
		||	pFrame[17] < 8000.	)
	{
		return;
	}

	if (   sortOn.max1 * 3. > sortBurst.min1
		|| sortOn.max1 > 300. )
	{
		return;
	}

	int nMisc = 0;	// Number of chars put into pMisc.
	int bEndpoint = (nFrameL > 18);

	int digitcnt = nFrameL > 18 ? nFrameL - 10 : nFrameL - 2;

	double dur[16];
	double* d = dur + digitcnt - 1;
	float* f = pFrame + 2*digitcnt + 1;
	float* skip = pFrame + 17;
	do
	{
		if ( f != skip )
		{
			*d = f[0] + f[-1];
			--d;
		}
		f -= 2;
	} while ( f > pFrame );

	// at ideal scale, bursts 0-15 scale to 7-22
	double scale = 6.51 / sortBurst.min1; // smallest scale that will give 0-burst as 7
	double best = 1.;
	double bestScale;
	double bestH;

	for (;;)
	{
		double tot = 0;     // (Integer stored as double to reduce number of double->int conversions)
		double check = 0;
		double nextScale = 999.;
		double* d = dur + digitcnt - 1;//15;
		for (;;)
		{
			double x = (*d) * scale;
			double n = floor( x + .5 );     // (Integer stored as double to reduce number of double->int conversions)
			if ( n > 22. )
				goto finished;			// Scale too large, burst 15 should only give n=22
			tot += n;
			double s = (n+.501) / (*d);	// Smallest scale that will increase digit nearest to scaled value for current burst by 1
			if ( s < nextScale )
				nextScale = s;			// Smallest scale that will increase digit nearest to scaled value for some burst by 1 
			if ( (--d) < dur  || d == dur+7 )
			{
				int ch = (int)tot-8;	// ch is 6*8 more than nibble sum, and 6*8 is masked out by the &15 below
				tot = 0.;
				if ( ( ch & 15 ) == 0	// Tests checksum for first half-frame, or second half-frame provided it has
					&& ( d < dur || digitcnt == 16 ))	// no missing digits.  Value should be 0.
					check -= .05;		// if correct checksum, reduce check contrib to score
				if ( d < dur )
					break;
			}
		}

		double high;

		for (;;)
		{
			double low_d;
			high = 0;
			double low = 0;
			double* d = dur + digitcnt - 1;//15;
			do
			{
				double x = (*d) * scale;
				double n = floor( x + .5 );
				x -= n;			// x is now error (between -0.5 and +0.4999) from true value 
				if ( x > high )	// get the extreme error values
				{
					high = x;
				}
				else if ( x < low )
				{
					low = x;
					low_d = *d;	// *d of burst with most negative error
				}

			} while ( (--d) >= dur );
			if ( high + low >= -.0001 ) // If median error negative, scale can be increased a little. 
				break;					// Break if median error positive, or almost so.			
			double maxStep = 1.;
			d = dur + digitcnt - 1;//15;
			do										// This calculates scale increase needed to bring
			{										// median error to zero, without changing the digit
				double x = (*d) * scale;			// to which each burst will be rounded.
				double n = floor( x + .5 );
				double s = (n-x-low)/((*d)+low_d);
				if ( s < maxStep )
					maxStep = s;
			} while ( (--d) >= dur );
			scale += maxStep;						// Adds this correction to the scale.
			// Will now repeat the loop and exit at the break, with final value of high.
		}

		double score = high + check;

		if ( score < best )	
		{
			best = score;
			bestH = high;
			bestScale = scale;
		}
		scale = nextScale;
	}
finished:
	newPreemptValue = prByXMP; 	// Avoid Gap decode of same frame
	newPreemptLength = nFrameL;

	char result[32];
	char* result2 = result+16;
	char* p = result;
	d = dur;
	do
	{
		double x = (*d) * bestScale;
		int n = (int)floor( x - 6.5 );
		*p++ = n;
	} while ( (++d) < dur+digitcnt/*16*/ );

	// Look to see if there is a following frame
	float* pFrameStart2 = pFrameEnd + (nFrameL > 18 ? - 17 : 1);
	float* pFrameEnd2 = pFrameStart2;
	for ( ; pFrameEnd2 < pMainLimit; pFrameEnd2++ )
	{
		if (	pFrameEnd2 > pFrameEnd + 18*(nFrameL <= 18)	// Make sure we don't only get first half 
			&&	*pFrameEnd2 > sortBurst.max2 * 3.
			&&	*pFrameEnd2 > 8000. ) break;
	};

	int nFrameL2 = (pFrameEnd2 - pFrameStart2 + 1 )/2;
	int digitcnt2 = min(nFrameL2 - 2, 16);	// May be corrected later

	if ( nFrameL2 > 15 && pFrameStart2[17] > sortBurst.max2 * 3.)
	{		
		double* d = dur + digitcnt2 - 1;
		float* f = pFrameStart2 + 2*digitcnt2 + 1;
		float* skip = pFrameStart2 + 17;
		do
		{
			if ( f != skip )
			{
				*d = f[0] + f[-1];
				--d;
			}
			f -= 2;
		} while ( f > pFrameStart2 );

		p = result2;
		d = dur;
		do
		{
			double x = (*d) * bestScale;
			int n = (int)floor( x - 6.5 );
			*p++ = n;
		} while ( (++d) < dur+digitcnt2 );

		if (nFrameL > 18 && nFrameL2 >= 18 && nFrameL2 <= 26)
		{
			if (*pFrameEnd2 < nMaxDur * 2)
			{
				// True leadout is missing but we reached intermediate leadout in frame 2
				digitcnt2 = nFrameL2 - 10;
			}
			else
			{
				// True leadout is missing and signal ended before next intermediate leadout
				// so determine digitcnt2 by matching the four digits of F.  If there is no
				// match then the 2nd frame is of no use.
				while (		digitcnt2 > 0 
						&&	!(	result2[digitcnt2-1] == result[digitcnt-1]
							&&	result2[digitcnt2-2] == result[digitcnt-2]
							&& result2[digitcnt2-3] == result[digitcnt-3]
							&& result2[digitcnt2-4] == result[digitcnt-4] ) )
				{
					digitcnt2--;
				}
			}
			if (digitcnt2 < 14) // Too few digits to be useful
				digitcnt2 = 0;
		}
	}
	else digitcnt2 = 0;

	// sprintf(pMisc, "D1 = %d, D2 = %d, L2 = %d", digitcnt, digitcnt2, nFrameL2);

	// Count the number of occurrences of each digit in each whole frame and in both halves of first frame.
	// The whole frame counts will remain based on original data, the half frame counts are modified if
	// changes are made to the first frame.
	int i, charcnt[16], charcnt1[16], charcnt2[16], charcntF2[16];
	for (i = 0; i < 16; i++) {charcnt[i] = 0; charcnt1[i] = 0; charcnt2[i] = 0; charcntF2[i] = 0; }
	for (i = 0; i < digitcnt; i++) 
	{
		charcnt[result[i]]++;
		if (i < 8) charcnt1[result[i]]++;
		if (i >= 8) charcnt2[result[i]]++;
	}
	for (i = 0; i < digitcnt2; i++) 
	{
		charcntF2[result2[i]]++;
	}

	int nFirstFrame = -1; // 1 if frame is a first frame, 0 if it is a second frame, -1 undetermined.
	int bRecovered = false;
	if (digitcnt2)
	{
		// If digit 8 never occurs and 2nd frame has less than 16 digits,
		// insert it in its fixed position.
		if (charcnt[8] == 0 && charcntF2[8] == 0 && digitcnt2 < 16)
		{
			for (i = 15; i > 10; i--) result2[i] = result2[i-1];
			result2[10] = 8;
			digitcnt2++;
		}
		char c1 = 0, c2 = 0;
		for (i = 8; i < digitcnt; i++) c1 += result[i]; 
		for (i = 8; i < digitcnt2; i++) c2 += result2[i];
		c1 &= 15; c2 &= 15;
		// If 2nd frame is complete but 1st frame is missing a digit,
		// copy the 2nd half frame from 2nd frame to 1st.
		// Also do so if both frames complete and chksum for 2nd half of 1st frame is not valid but
		// that for 2nd half of 2nd frame is either valid or that half has the smaller check digit
		// and so is most likely to be correct.
		if (digitcnt2 == 16 && result2[10] == 8 && (digitcnt == 15 || digitcnt == 16 && c1 != 0 ) )
		{
			// Before overwriting, determine if frame is a first frame.
			// Second version final part checks to see if the error is in the checksum digit.
			if (digitcnt == 16)
			{
				nFirstFrame = (result[10] == 0);
				if (nFirstFrame)
				{
					int truechk = result2[9]^8;
					// nMisc += sprintf(pMisc+nMisc, " truechk = %d present = %d", truechk, (result[9] - c1) & 15);
					if (	(c1 == 1 || c1 == 15) && c2 == 0	
						&&	truechk == ((result[9] - c1) & 15)
						&&	truechk > 8		// Only digits > 8 appear to be subject to coalescence
						&&	charcnt[truechk] == 0 && charcntF2[truechk] == 0
						&&	charcnt[result[9]] == 1 && charcntF2[result[9]] == 0 )
					{
						// Burst for result[9] appears to have been misidentified and only
						// occurs once, so correct the identification.
						charcnt[result[9]] = 0;
						charcnt[truechk] = 1;
						result[9] = truechk;
						c1 = 0;
						bRecovered = true;
					}
				}
			}
			else
			{
				nFirstFrame = (result[9] == 0 && charcnt[result2[9]^8] == 0
				// check digit missing and its true value does not occur
				|| result[10] == 0 && ((result[9] - result2[9] - c1) & 15) == 8
				// error is in check digit and the toggle nibble is 0
				);
			}
			// If error not yet resolved and 2nd half of 2nd frame has valid chksum, 
			// or smaller check digit, copy second half frame from 2nd frame
			if (	c1 
				&& (	!c2 
					||	digitcnt == 16 && nFirstFrame && result[9] > result2[9]) )
			{
				for (i = 8; i < 16; i++) { result[i] = result2[i]; }
				digitcnt = 16;
				bRecovered = true;
			}
			// Recalculate charcnt2
			for (i = 0; i < 16; i++) charcnt2[i] = 0;
			for (i = 8; i < 16; i++) charcnt2[result[i]]++;

		}
	}

	// Calculate checksum in each half of first frame.
	char chk1 = 0, chk2 = 0, newchar = 0;
	int bCorrected = false;
	int bCalculated = false;
	for (i = 0; i < 8; i++) chk1 += result[i];
	for (i = 8; i < digitcnt; i++) chk2 += result[i];
	chk1 &= 15;
	chk2 &= 15;

	// Test first half of first frame for coalescence of E and F, knowing that correctly result[3] = 0xF.
	if (	digitcnt == 16 && (chk1 == 1 || chk1 == 15) 
		&&	(	newchar = 14 + (chk1 == 15),
				result[3] == (newchar^1)
			&&	charcnt1[newchar^1] == 2 
			&&	charcnt[newchar] == 0 
			&&	charcntF2[newchar] == 0 ) )
	{
		// The check nibble F appears to have coalesced with a single E in the first 
		// half-frame, so correct the erroneous character.
		for (i = 0; i < 8; i++)
		{
			if (result[i] == (newchar^1) && (i == 3) == (newchar == 15)) result[i] = newchar;
		}
		charcnt1[15] = 1;
		charcnt1[14] = 1;
		chk1 = 0;
		bCorrected = true;
	}
	
	if (	digitcnt == 16
		&&	(chk1 == 0 || chk2 == 0 || chk1 == chk2)
		&&	(chk1 == 0 || chk1 == 1 || chk1 == 15)
		&&	(chk2 == 0 || chk2 == 1 || chk2 == 15)
		&&	!(chk1 == 0 && chk2 == 0)
/*		&&	(	(chk1 == 0 && (chk2 == 1 || chk2 == 15))
			||	(chk2 == 0 && (chk1 == 1 || chk1 == 15)) )*/
		)
	{
		// A correction is needed, as there are no missing digits and the checksum of 
		// at least one half frame is out by 1.

		// Seek digits > 8 which occur more than once, so could be two values coalesced.
		// Test if such a digit occurs exactly once in the half frames that are in error.
		// Construct the replacement digit (newchar) that would correct the checksum error.
		// Test if this does not occur at all in the original data.
		// If all these conditions are met then this is a possible correction.
		// It is most likely to be correct for the largest digit meeting these conditions.
		// Seek the largest such digit, if any, and if it exists, make the correction. 

		i = 15; 
		while (		i > 8 
				&& !(	(charcnt[i] > 1 || charcntF2[i] > 1)
						&&	(	chk2 && (charcnt2[i] == 1 || charcnt2[i] == 2
									&& result[12] == result[13] && result[14] == result[15]
									&& (result[12] == 0 && result[14] == i || result[12] == i && result[14] == 0) )
								&&	(newchar = i + 1 - 2 * (chk2 == 1), 
									newchar < 16 && charcnt[newchar] == 0 && charcntF2[newchar] == 0)
							||	chk1 && charcnt1[i] == 1
								&&	(newchar = i + 1 - 2 * (chk1 == 1), 
									newchar < 16 && charcnt[newchar] == 0 && charcntF2[newchar] == 0)
							)
					)
			) i--;
		if ( i > 8 && charcnt1[i] == 1 && chk1 == ((i-newchar)&15) )
			for (int m=0; m<8; m++)
				if (result[m] == i) {result[m] = newchar; bCorrected = true; chk1 = 0;}
		// For 2nd half frame only correct the value closest to the end of the frame.
		if ( i > 8 && charcnt2[i] > 0 && chk2 == ((i-newchar)&15) )
			for (int m=15; m>=8; m--)
				if (result[m] == i) 
				{
					result[m] = newchar;
					if (charcnt2[i] == 1) bCorrected = true;
					else bCalculated = true;
					chk2 = 0; 
					break;
				}
	}

	// if a single digit is missing from 2nd half of both frames, try to calculate it.
	if (	digitcnt == 15 && digitcnt2 == 15
		&&	result[8] == result[0] && result2[8] == result[0]
		&&	(result[10] == 0 || result[10] == 8) && result2[10] == 8
		&&  result[11] == result[2] && result2[11] == result[2]
		&&  result[12] == result2[12] && result[13] == result2[13] && result[14] == result2[14] )
	{
		// A digit is missing from the four digits of F.  Calculate it from the checksums.
		char c1 = 0, c2 = 0;
		for (i = 8; i < 15; i++) c1 += result[i]; 
		for (i = 8; i < 15; i++) c2 += result2[i];
		c1 = (16 - c1) & 15;	// missing digit calculated from frame 1
		c2 = (16 - c2) & 15;	// missing digit calculated from frame 2
		int poss1 = (charcnt[c1] == 0 && charcntF2[c1] == 0);
		int poss2 = (charcnt[c2] == 0 && charcntF2[c2] == 0);
		int newchar = 0;
		if (c1 == c2 && poss1) newchar = c1;
		if (c1 != c2 && poss1 != poss2) newchar = poss1 ? c1 : c2;
		// if different and both possible, choose result from frame with smaller check digit
		// as the check digit most likely to be correct.
		if (c1 != c2 && poss1 && poss2) newchar = result[9] < result2[9] ? c1 : c2;
		// Now find where to put it.  It is likely to be in the 2nd posn in one of the OBC bytes
		// than the first, as if both were "new" digits and one could be allocated a learned burst
		// then the first one would be the one allocated.
		if (result[12] == 0 && result[13] == 0) { result[15] = newchar; bCalculated = true;}
		if (result[13] == 0 && result[14] == 0) { result[15] = 0; result[13] = newchar; bCalculated = true;}
		if (bCalculated)
		{
			// Correct the check digit, as it may be wrong if c2 was used.
			result[9] = (result[9] + c1 - newchar) & 15;
			digitcnt = 16;
			chk2 = 0;
		}
	}

	// If two digits are missing from 2nd half of first frame, but it looks like a second frame due to the
	// fixed digit 8, and if (a) the checksum is zero and (b) there are no zeroes in the data, then it looks
	// as if one of the two OBC values is zero but the zeroes are missing.  We can't tell which OBC it is,
	// so make OBC2 be zero.

	int bCalculated2 = false;
	if ( digitcnt == 14 && chk2 == 0 && charcnt[0] == 0 && result[10] == 8 )
	{
		result[14] = 0; result[15] = 0;
		digitcnt = 16;
		bCalculated2 = true;
	}

	if (result[3] != 15 && result[3] != 14) 
	{
		// This is an additional safeguard against an XMP decode arising spuriously.
		// The correct value is 15 but an uncorrected coalescence could show it as 14.
		return;
	}

	if (   chk1 == 0 && chk2 == 0	// condition that both checksums are correct
		&& digitcnt == 16
		&& result[3] == 15
		&& result[8] == result[0]
		&& (result[10] &~9) == 0	// possible values are 0, 8, 9
		&& result[11] == result[2]
        )
	{
		if (nFirstFrame < 0) nFirstFrame = (result[10] == 0);
		int bFinalFrame = false;

		// Code to check if there is a final frame with toggle nibble = 9
		// First get true length of repeat frames (we may be starting at a repeat frame)
		int nFrameL3 = (nFirstFrame) ? digitcnt2 + 1 + (nFrameL2 <= 18) : digitcnt + 1 + (nFrameL <= 18);
//		int nFrameL3 = digitcnt2 + 1 + (nFrameL2 <= 18);	
		float* f2 = (nFirstFrame) ? pFrameStart2 : pFrame;
		float* f3 = f2 + 2*nFrameL3;
//		float* pFrameStart3 = pFrameStart2 + 2*nFrameL3;
		i = 0;

//		float* f2 = pFrameStart2;
//		float* f3 = pFrameStart3;
		int chk3 = 0, tog3 = 0, nxt = 0;
		int reqdchk = (nFirstFrame) ? result2[9] : result[9];
		reqdchk = (reqdchk - 1) & 15;
		while (		f3 + i < pFullLimit - 2
				&&	(	// Bypass leadout before final frame if	leadouts missing elsewhere.
						f3 += 2*(nFrameL > 18 && i%(2*nFrameL3) == 0 && f2[i+1] < 8000. && f3[i+1] > 8000.),	
						f2[i+1] > 8000. && f3[i+1] > 8000.			// Both are lead-outs	
					||	floor((f2[i] + f2[i + 1])*bestScale - 6.5)	// Digits match
							== floor((f3[i] + f3[i + 1])*bestScale - 6.5) )
			) i += 2;
		if (	f3 + i < pFullLimit - 6	// There is a difference with at least two more bursts to follow
			&&	i%(2*nFrameL3) >= 18		// Test if we are past the half-frame separator
			&&	(	chk3 = (int)floor((f3[i] + f3[i+1])*bestScale - 6.5),
					tog3 = (int)floor((f3[i+2] + f3[i+3])*bestScale - 6.5),
					nxt = (int)floor((f3[i+4] + f3[i+5])*bestScale - 6.5),
						// First deal with case of no missing char in 2nd half of final frame
					(f3[i-3] > 8000. && chk3 == reqdchk && nxt == result2[11] 
						|| f3[i-5] > 8000. && chk3 == 9 && tog3 == result2[11] && nxt == result2[12])  
					||	//Now deal with case when there is a missing char
						(chk3 == 9 && charcnt[reqdchk] == 0 && charcntF2[reqdchk] == 0
						|| chk3 == reqdchk && charcnt[9] == 0 && charcntF2[9] == 0 )
					&& f3[i-3] > 8000. && tog3 == result2[11] && nxt == result2[12] ) )
		{
			bFinalFrame = true;
		}
		else if ( result[10] == 9 )	// Last frame itself
		{
			bFinalFrame = true;
		}
		else if ( !framed(nMaxDur*2) && nFrameL <= 18 && result2[10] <= 8 )
		{
			// Only allowed a short lead-out when there is a final frame with toggle nibble 9.
			// But since 9 can be coalesced with A, allow it if toggle nibble > 8.
			*pMisc = 0;
			return;
		}
		// End of final frame check

		// nMisc += sprintf(pMisc+nMisc, "i = %d; chk3 = %d; reqdchk = %d; tog3 = %d, nFrameL2 = %d", i, chk3, reqdchk, tog3, nFrameL2);
		int OBC1 = (result[12]<<4) + result[13];
		int OBC2 = (result[14]<<4) + result[15];
		int OEM = (result[4]<<4) + result[5];
		*pDevice = (result[6]<<4) + result[7];
		*pSubDevice = result[0]*16 + result[2];
		strcpy( pProtocol, "XMP" );

		if ( bFinalFrame )
		{
			nMisc += sprintf(pMisc+nMisc, "%sWith Final Frame.", (nMisc) ? " " : "");
		}

		if ( OEM != 68 )
		{
			nMisc += sprintf(pMisc+nMisc, "%sOEM = %d", (nMisc) ? " " : "", OEM);
		}

		if (OBC1 || !OBC1 && !OBC2)
		{
			*pOBC = OBC1;
			pHex[0] = OBC1;
			if (OBC2) 
			{
				nMisc += sprintf(pMisc+nMisc, "%sOBC2 = %d Hex2 = %02X", (nMisc) ? " " : "", OBC2, OBC2);
			}
			else 
			{
				strcat( pProtocol, "-1");
			}
			if (!OBC1)
			{				
				strcat( pProtocol, "/2");
			}
		}
		else if (OBC2)
		{
			*pOBC = OBC2;
			pHex[0] = OBC2;
			strcat( pProtocol, "-2");
		}

		bXMPHalfValid = true;
		nNote_out = ( nFirstFrame ) ? 4 : (nAuxNote_out = 1, 1);
		if ( bFinalFrame && nNote_out == 4 ) nNote_out = 6;
		else if ( bFinalFrame && nNote_out == 1 ) nNote_out = 5;
	}

	int bComma = false;
	int bModified = bEndpoint | bRecovered | bCorrected | bCalculated | bCalculated2;
	if ( bModified ) nMisc += sprintf(pMisc+nMisc, "%s(", (nMisc) ? " " : "");
	if ( bEndpoint )
	{
		nMisc += sprintf(pMisc+nMisc, "End");
		bComma = true;
	}
	if ( bRecovered )
	{
		nMisc += sprintf(pMisc+nMisc, "%sRec", (bComma) ? ", " : "");
		bComma = true;
	}
	if ( bCorrected )
	{
		nMisc += sprintf(pMisc+nMisc, "%sCor", (bComma) ? ", " : "");
		bComma = true;
	}
	if ( bCalculated )
	{
		nMisc += sprintf(pMisc+nMisc, "%sCal", (bComma) ? ", " : "");
		bComma = true;
	}
	if ( bCalculated2 )
	{
		nMisc += sprintf(pMisc+nMisc, "%sCal2", (bComma) ? ", " : "");
	}
	if (bModified) nMisc += sprintf(pMisc+nMisc, ")");
	if (*pProtocol) return;
    //See if this is Velodyne   DAR July 2011
	if ( chk1 == 7 && chk2 == 7	// condition that both checksums are correct
		&& digitcnt == 16
		&& result[0] == result[8]   //subdevice hi nibble
		&& result[2] == result[10]  //subdevice lo nibble
		&& result[3] == 15 && result[11] == 15
		&& result[4] == result[12]	//device
		&& result[6] == result[14]  //function hi nibble
		&& result[7] == result[15]  //function lo nibble
		)
	{
		*pOBC = result[6]*16 + result[7];
		*pDevice = result[4];
		*pSubDevice = result[0]*16 + result[2];
		strcpy( pProtocol, "Velodyne" );
		return;
	}
	p = result + digitcnt;
	*p = 0;	// Puts the terminating zero in the result string.
	do
	{
		--p;
		*p = "0123456789ABCDEF"[*p];
	} while (p > result);
	sprintf(pProtocol, "XMP:%d.%03d-%s", (int)(.5+1./bestScale), (int)(.5+1000.*bestH), result);
	if (chk1 == 0) bXMPHalfValid = true;	// used to prevent spurious Mitsubishi decode
			
}  // tryXMP

void Signal::tryZenith()
// Half bits are either 1,-10 or 1,-1,1,-8
// There is an initial independent half bit, followed by full bits, where each
// full bit is a half bit and its complement.
// Each full bit is this 3 bursts long.  So the whole is either 3N+1 or 3N+2
// bursts, depending on the first half bit.  N is at least 5.
{
	// {40K,520}<1,-1,1,-8|1,-10>( S:1,<1:2|2:2>( F:D ),-90m )+ //OLD
	// {40K,520,msb}<1,-10|1,-1,1,-8>( S:1,<1:2|2:2>( F:D ),-90m )+ //DAR July 2011  This appears to be the correct IRP


	// MaxDur should be 10*520.  Use 10.5*520 for cut off.
	// Lead Out should be greater than 90000.  90000/(10.5*520)=16.5
	// But the CML files I checked had a lot of short LeadOuts, so I cut the ratio
	if (   nFrameL < 16
		|| ! framed(sortOff.max1*4) )
	{
		return;
	}

	int nStyle = nFrameL % 3;	// 1 if Start is 1,-10.  2 if start is 1,-1,1,-8
	if (nStyle == 0)
		return;
	if (nStyle == 1)
	{
        // First burst and largest burst must both be 1,-10 
        
		if ( sortBurst.max1 * (.9*10./11.) > pFrame[1] )
			return;
	}
	else // nStyle == 2
	{
        // First burst is 1,-1
        // Second burst is 1,-8
        // Largest burst is 1,-10

		if ( sortBurst.max1 * (.9*8./11.) > pFrame[3] ) // -8 too small
			return;
        if ( (pFrame[0]+pFrame[1]+pFrame[2]+pFrame[3])*.9 > sortBurst.max1 )
            return;
	}

    if ( sortOn.max1*6 > sortOff.max1 )
		return;			// One of the ON values is too big
	int nBits = nFrameL / 3;
	if (nBits > sizeof(cBits)*8)
		return;

    cleanup();
    burst pnt = pFrame + nStyle * 2;
    do
    {
        double x = pnt[0] + pnt[1];
        double y = pnt[2] + pnt[3];
        double z = pnt[4] + pnt[5];
        if ( x > y )
        {
            // x nominal 11 // y nominal 2 // z nominal 9 or last
            require( y*4. < z )
        }
        else
        {
            // x nominal 2 // y nominal 9 // z nominal 11 or last
            require( x*4 < y )
            require( y*.9 + x < z )
			cBits[nBit>>3] |= 0x80 >> (nBit & 7);
        }
        ++nBit;
        pnt += 6;
    } while ( pnt < pFrameEnd );

	*pDevice = nBits;
	*pSubDevice = nStyle-1;
    *pOBC = cBits[0];
	if (*pDevice < 8)
	{
		newPreemptValue = prByZenith; 	// Avoid Gap decode of same frame
		newPreemptLength = nFrameL;
        *pHex = *pOBC >> 1;
		*pOBC >>= 8 - *pDevice;
		if (*pSubDevice)
			*pHex += 0x80;
	}
	else if (*pDevice > 8)
	{
        unsigned char* cb=cBits+1;
		sprintf(pMisc, "E=%X", *cb);
		while ( (nBits -= 8) > 8)
			sprintf(pMisc+strlen(pMisc), ".%X", *++cb);
	}
	strcpy(pProtocol, "Zenith");
}  // tryZenith

// pBit[0] = Lead out Off
// pBit[1] = Lead in On (1)
// pBit[2] = Lead in Off (5)
// pBit[3] = First half bit (1)
int Signal::moreBlaupunkt(int bits)
{
	if (   pBit+3 >= pMainLimit
		|| pBit[2] <= sortBurst.max2
		|| pBit[2] > sortBurst.max1
		|| pBit[1] > nMaxShort
		|| pBit[3] > nMaxShort )
	{
		return -1;
	}
	float* save_pBit = pBit;
	cleanup();
	pBit = save_pBit + 4;      // Middle of first bit
	nState = 1;
	for (;;)
	{
		if ( pBit >= pFullLimit )
			break;
		if ( ! phaseBit() )
			break;
		if ( nBit == bits )
		{
			pBit = ((pBit-pDuration)|1)+pDuration;
			if (pBit < pFullLimit && pBit[0] < sortBurst.max1)
				break;
			return getLsb(0,bits);
		}
	}
	pBit = save_pBit;
	return -1;
}

void Signal::tryXX()	// {500}<-1,1|1,-1>((1,-5, 1:1, 
{
    if ( pFrameEnd[0] < sortBurst.max1 )    // Right hand framing
        return;
	if ( pFrame[1] <= sortBurst.max2 )      // Lead-in
		return;
	if ( nFrameL > 20 || nFrameL < 6 )
		return;
	if ( preemptValue >= prBlaupunkt )
		return;
	if ( sortBurst.min1*4 < sortBurst.max1 )
		return;
	cleanup();
	float u = sortBurst.max1 * (1./18);
    minGlitch = .5*u;
	nMinShort = nMinShort2 = 2*u;
	nMaxShort = nMaxShort2 = 4*u;
	nMinLong = 5*u;
	nMaxLong = 7*u;
	pBit += 2;
	if (*(pBit++) > nMaxShort)
		return;
	nState = 1;
	do
	{
		if ( ! phaseBit() )
			return;
	} while (pBit != pFrameEnd);
	if (nBit < 8)
		return;
	switch (nBit)
	{
	case 9:
	{
		newPreemptValue = prBlaupunkt;
		newPreemptLength = nFrameL;
		int obc = getLsb(0,7);
		int device = getLsb(7,2);
		strcpy(pProtocol, "Blaupunkt{body}");
		if (getLsb(0,9)==511)
		{
			int command = moreBlaupunkt(9);
			if (command>=0 && command != 511)
			{
				int cmds = 0;
				obc = getLsb(0,6);
				device = getLsb(6,3);
				int nextCommand;
				do
				{
					newPreemptLength = (pBit-pFrame)/2;
					++cmds;
					nextCommand = moreBlaupunkt(9);
				} while (nextCommand == command);
				strcpy(pProtocol, "Blaupunkt");
				if (cmds>1)
					sprintf(pMisc, "(%d)", cmds);
			}
			else
			{
				strcpy(pProtocol, "Blaupunkt{prefix}");
				return;
			}
		}
		*pOBC = obc;
		*pDevice = device;
		*pHex = (msb(obc)>>1) + 0x80;
		return;
	}
	case 15:
	case 16:
	{
		newPreemptValue = prBlaupunkt;
		newPreemptLength = nFrameL;
		int obc = getLsb(0,8);
		int device = getLsb(8,nBit-1);
		sprintf(pProtocol, "NRC%d{body}", nBit+1);
		char* pP = pProtocol+5;
		if ( getLsb(0,nBit) == (1<<nBit)-2 )
		{
			int command = moreBlaupunkt(nBit);
			if (command>=0 && command != (1<<nBit)-2)
			{
				*pP = 0;
				int cmds = 0;
				obc = getLsb(0,8);
				device = getLsb(8,nBit-8);
				int nextCommand;
				do
				{
					newPreemptLength = (pBit-pFrame)/2;
					++cmds;
					nextCommand = moreBlaupunkt(nBit);
				} while (nextCommand == command);
				if (cmds>1)
					sprintf(pMisc, "(%d)", cmds);
			}
			else
			{
				strcpy(pP, "{prefix}");
				return;
			}
		}
		*pOBC = obc;
		*pDevice = device;
		return;
	}
	}
	strcpy(pProtocol, "XX");
	*pDevice = nBit;
	char *pt=pMisc;
	for (int ndx= (nBit+7)>>3; --ndx>=0; )
	{
		sprintf(pt,"%02X", cBits[ndx]);
		pt+=2;
	}

}

void Signal::tryGXB()
// GXB  {38.3k,520}<1,-3|3,-1>(1,-1,D:4,F:8,P:1,1,^???)+
// P is odd parity for F
{
	if (nFrameL != 15)
		return;
	if (   sortBurst.min2 != sortBurst.mid2
		|| sortBurst.min1 >= sortOn.max1
		|| sortBurst.min1 != pFrame[0] + pFrame[1]
		|| sortBurst.min1*1.5 > sortBurst.mid2
		|| sortBurst.mid2*1.25 < sortBurst.max1 )
	{
		return;
	}
	nMaxShort = sortBurst.mid2*.5;
	cleanup();
	pBit+=2;
	decodeX( 13 );
	makeMsb();
	*pDevice = getMsb(0,4);
	*pOBC = getMsb(4,8);
	if ( parity( getMsb(4,9) ) )
	{
		strcpy(pProtocol, "GXB");
	}
	else
	{
		sprintf(pProtocol, "GXB-%03X.%d", getMsb(0,12), getMsb(12,1));
	}


}  // tryGXB

void Signal::trySingleBurstSize()
{
    // pid-0003 16 bits, no end pulse

    if ( nFrameL != 16 )
        return;

    if ( sortBurst.min1 * 1.3 < sortBurst.max1 )
        return;

    if ( sortOn.min1 * 1.4 < sortOn.max1 )
        nMaxShort = (sortOn.min1 + sortOn.max1) * .5;
    else
        nMaxShort = (sortBurst.min1 + sortBurst.max1) * .25;
    cleanup();
    while ( nBit <= nFrameL )
    {
        if ( pBit[0] > nMaxShort )
        {
			cBits[nBit>>3] |= 1 << (nBit & 7);
        }
        pBit += 2;
        nBit++;
    }

    switch ( nFrameL )
    {
        case 16:
            if ( cBits[0] + cBits[1] == 255 )
            {
                // {40.2k,389}<2,-2|3,-1>(F:8,~F:8,^102k)+
                //
        		strcpy(pProtocol, "pid-0003");
            	*pOBC = cBits[0];
                *pHex = msb(*pOBC);

				newPreemptValue = prByPid_0001;    // Avoid Async decode
				newPreemptLength = 16;
            }
            return;
    }

}  // trySingleBurstSize

void Signal::trySony()
// Sony8  {40k,600}<1,-1|2,-1>(4,-1,F:8,^22200)
// Sony12 {40k,600}<1,-1|2,-1>(4,-1,F:7,D:5,^45m)+
// Sony15 {40k,600}<1,-1|2,-1>(4,-1,F:7,D:8,^45m)+
// Sony20 {40k,600}<1,-1|2,-1>(4,-1,F:7,D:5,S:8,^45m)+
{

	switch (nFrameL)
	{
		case 9:
            require( sortBurst.max2 < 3.3*600 )
            require( sortBurst.max1 < 5.5*600 )
			// max content = .6*(5+8*3) = 17.4m
			// min leadout = (22.2 - 17.4)/.6 = 8 units
			require( framed( max( nMaxDur*1.5, pFrame[1]*6.) ) )
			break;
		case 13:
			// Max content = .6*(5+12*3) = 24.6ms
			// Min leadout = (45-24.6)/.6 = 34 units
			require( framed(nMaxDur*7) )
			break;
		case 16:
			// Max content = .6*(5+15*3) = 30ms
			// Min leadout = (45-30)/.6 = 25 units
			require( framed( max( nMaxDur*1.5, pFrame[1]*6.) ) )
			break;
		case 21:
			// Max content = .6*(5+20*3) = 39ms
			// Min leadout = (45-39)/.6 = 10 units
			require( framed(nMaxDur*2) )
			break;
		default:
			require( false );
	}
	require( sortOff.min1 > 0.8*600)  // DAR 2012 avoid misdecode from Humax 4Phase
    float bigBurst = sortBurst.max2;
    float smallBurst = sortBurst.min1;
    float firstBurst = pFrame[0]+pFrame[1];
    if ( firstBurst > bigBurst )
    {
        require( sortBurst.max1 * (2. / 6) < smallBurst )  // Lead in too big
    }
    else
    {
        if ( firstBurst < sortBurst.min2 )
            smallBurst = sortBurst.min2;
        bigBurst = sortBurst.max1;
        require( bigBurst < 2350. )  // Avoid misdecode from RC5
    }

    require( bigBurst * (2. / 3.6) < smallBurst )  // Larger burst too big

    require( sortOff.min1*1.6 > sortOff.max1 )  // All but terminal gap should be the same size

                                    // bigBurst is nominal 3
	nMaxShort = bigBurst*(5./6);    // nMaxShort is nominal 2.5

	// If all the pulses are the same size, fall back on expected timing
	if ( nMaxShort < smallBurst )
	{
		require( pFrame[0]+pFrame[1] > 3.3*600 )
		require( bigBurst < 3.3*600 )
        require( smallBurst > 1.5*600 )
		nMaxShort = 1500;
	}

	cleanup();
	pBit+=2;
	decodeX2( nFrameL-2 );  // Can't use off half of last bit
	if ( pBit[0]*(2.5/1.33) > nMaxShort )   // Last On is nominal 1 or 2, but bias for battery problems
	{
		cBits[nBit>>3] |= 1 << (nBit & 7);
	}
	
	sprintf(pProtocol, "Sony%d", nFrameL-1);
	if (nFrameL == 9)
	{
		*pOBC = cBits[0];
		return;
	}
	newPreemptValue = prBySony; 	// Avoid Async decode of same frame
	newPreemptLength = nFrameL;
	*pOBC = getLsb(0,7);
	pHex[0] = msb(*pOBC);
	if (nFrameL <= 16 )
	{
		*pDevice = getLsb(7, nFrameL-8);
		pHex[1] = pHex[0]+1;
	}
	else
	{
		*pDevice = getLsb(7, 5);
		*pSubDevice = getLsb(12, 8);
	}
} // trySony

void Signal::tryAsync()
{
	return;     //DAR 2012  inspired by false decodes of Humax 4Phase.
				// hifi-remote.com has dozens of posts referring to false decodes, without any genuine decodes reported
	if ( nFrameL < 5 )
		return;
	if (!framed(nMaxDur*4))
		return;
	if (preemptValue >= prAsync)
		return;

    double minDur = sortOff.min1;
    if ( minDur > sortOn.min1 ) minDur = sortOn.min1;
    double minUsPerBit  = nMaxDur * (1/9.3);
    double maxUsPerBit  = nTotDur * (1/31.);
    double maxUsPerBit2 = minDur * (1/.7);
    if ( maxUsPerBit > maxUsPerBit2 ) maxUsPerBit = maxUsPerBit2;
    if ( maxUsPerBit < minUsPerBit )
        return;

	int result = decodeAsync(pFrame, 0, 0, minUsPerBit, maxUsPerBit, 10, 40);
	if (result)
	{
		sprintf(pProtocol, "Async%d:%.0f-%.0f:%02X.%02X..%02X.%02X", result, nMinShort, nMaxShort,
			cBits[0], cBits[1], cBits[result-2], cBits[result-1]);
		int ff = result>>1;
		*pDevice = cBits[ ff - 1 ];
		*pSubDevice = cBits[ ff ];
		*pOBC = cBits[ ff + 1 ];
		sprintf(pMisc,"%02X", cBits[0]);
		for (int n=1; n<result; n++)
			sprintf(pMisc+strlen(pMisc), " %02X", cBits[n]);
	}
} // tryAsync

void Signal::tryAirboard()
{
	if ( nFrameL < 5 )
		return;
	// Left framing always checked
	if (!framedLeft(22*8*105))
		return;
	// Right framing only checked if outside repeat section
	if (   *pFrameEnd < 22*8*105
		&& pFrameEnd < pDuration+2*nSingle )
	{
		return;
	}
	if (pFrame[2] < 5*105 || pFrame[2] > 8*105)
		return;
	if (nMaxDur > 22*8*105 || nMaxDur < 2*8*105)
		return;
    require( sortOn.max1 < 8*105 )
    require( sortOn.min1 > 5*105 || sortOn.min1 == pFrame[0] )

    double rClock = 1.0 / 105;
	int repByte = -1;
	int bytes = 0;
	int phase = 0;
	cBits[0] = 0;
    float* pX;
	for (pX = pFrame+1; ; pX+=2)
	{
		if ( pX == pDuration+2*nSingle+1 && repByte < 0 )
		{
			repByte = 2*bytes;
			if ( phase )
				--repByte;
		}
		int nb = (int)( floor( *pX * rClock) ) + 2;  // Number of sequential one bits
		int np = (nb >> 3) + phase;                  // New phase after those one bits
		if (np >= 10)                                // End of valid byte
		{
			cBits[bytes] |= (0xFF)<<phase;
			phase = 0;
			cBits[++bytes] = 0;
			if ( pX > pFrameEnd )                // Found a valid byte past requested frame
				return;                          // so requested frame is wrong.
			if ( *pX >= 22*8*105 )
				break;
			if ( pX >= pDuration+2*(nSingle+nRepeat)-1 )
				break;
		}
		else
		{
			if ( pX == pFrameEnd )					 // Frame ends on non byte boundary
				return;                              // so requested frame is wrong.
			if ( np > 7 )                            // Stop bits are wrong
				break;                               //   stop and check if past valid frame
			if ( (nb & 7) >= 5 )                     // Not whole number of bits
				break;                               //   stop and check if past valid frame
			cBits[bytes] |= (1<<np) - (1<<phase);
			phase = np+1;
		}
	}
	if ( pX < pFrameEnd )
		return;
	sprintf(pProtocol, "AirB%d-", bytes);
	for (int nn=0; nn<bytes; ++nn)
	{
		sprintf(pProtocol+strlen(pProtocol),
			(nn==(repByte>>1))
			   ? ( (repByte & 1 ) ? ":%02X" : ";%02X" )
			   : ( nn ? ".%02X" : "%02X" )
			, cBits[nn] );
		*pOBC = cBits[0] & 7;
		*pDevice = cBits[0] >> 3;
	}
} // tryAirboard

// (37.7Khz,840)<1|-1>( 1,B:8,-2 ... )
void Signal::tryAirAsync()
{
	if (!framed(22*8*105))
		return;
	if ( nFrameL < 5 )
		return;
	if (preemptValue >= prAsync)
		return;

	// Longest > 22 bits is treated as a decode boundary
	// Longest < 2 (shortened) bits makes false decode too likely
	if (sortOff.max1 > 22*8*105 || sortOff.max1 < 2*7*105)
		return;
    if (sortOn.max1 > 9*9*105)
        return;

	int result = decodeAsync(pFrame, 0, 0, 7*105, 9*105, 11, 11);
	if (result)
	{
		sprintf(pProtocol, "AirAsync%d-", result);
		for (int nn=0; nn<result; ++nn)
		{
			sprintf(pProtocol+strlen(pProtocol),
				   ( nn ? ".%02X" : "%02X" )
				, cBits[nn] );
		}
	}

} // tryAirAsync

void Signal::tryAK()
{
	if ( nFrameL < 5 )
		return;
	if (!framed(10000) || nTotDur < 16*844 || nTotDur > 40*844)
		return;
	if (nMaxDur < 3*844 || nMaxDur > 10*844)
		return;
	if (sortOn.min1 < 300 || sortOn.max1 > 450)
		return;
	if (sortOff.min1 < 350 || sortOff.max1 > 6000)
		return;
	cleanup();
	strcpy(pProtocol, "AK-");
	char *pt = pProtocol+strlen(pProtocol);
	int count=0;
	for (float *pd=pFrame+1; pd<=pFrameEnd ;pd+=2)
	{
		int bt = (int)(*pd) / 844;
		*(pt++)='0';
		if (pd==pFrameEnd)
		{
			if (count >= 33)
				break;
			bt = 33 - count;
		}
		for (;;)
		{
			++count;
			if (count == 13 || count==21 || count==25 || count==33)
			{
				*(pt++)='-';
			}
			if (--bt < 0)
				break;
			*(pt++)='1';
			cBits[count>>3] |= 1 << (count & 7);
		}
	}
	*pt = 0;
	*pSubDevice = getLsb(7,3);
	*pDevice = getLsb(3,4);
	*pOBC = getLsb(13,7);
	*pHex = msb(*pOBC) + getLsb(21,1);

} // tryAK

void Signal::tryDirecTV()
{
	// PID 0162
	// GD:  The representation as 
	// {38.4k,601}<1,-1|1,-2|2,-1|2,-2>(10,-2,(D:4,F:8,C:4,1,-50,5,-2)+)
	// is missing "msb" and the freq of the 0162 executor is actually 38.1k.
	// A better representation is
	// {38k,600,msb}<1,-1|1,-2|2,-1|2,-2>(5,(5,-2,D:4,F:8,C:4,1,-50)+)
	// C = 7*(F:2:6)+5*(F:2:4)+3*(F:2:2)+(F:2)
	// since the final 5,-2 is not sent after the final leadout
	// Variants are freq = 38k, 40k or 57k, leadout = -50 or -15
	if (nFrameL != 10)
		return;
    if ( (pFrame[0]+pFrame[1]) * .66 < sortBurst.max2 )
        return;
    if ( pFrame[0] * .75 < sortOn.max2 )
        return;
//	if ( ! framed(sortBurst.max1 * 2) )
//  GD Above statement does not hold for first frame when leadout is -15
//  as then sortBurst.max1 = 12 and frame = 15.  We can weaken this without causing
//  problems since the checksum will rule out spurious decodes.  Note sortOn.max1 = 10.
	if ( ! framed(sortOn.max1 ) )
		return;

    nMaxShort = sortOn.max2 * .75;
    if ( nMaxShort < sortOn.min1 * 1.5 )
        nMaxShort = sortOn.min1 * 1.5;
    cBits[0] = cBits[1] = 0;
	// fill cBits left to right in order of bits received
    for (int ndx=16; --ndx >= 0; )
    {
        if ( pFrame[2+ndx] > nMaxShort )
            cBits[ndx>>3] += 0x80 >> (ndx & 7);
    }
    int cmd = getMsb(4,8);	// this corresponds to msb ordering
    int check = cmd - (cmd>>2) - 7*(cmd>>4) - 13*(cmd>>6);
    
    if ( getMsb(12,4) == (check&15) )
    {
		nNote_out = 11;
	    *pDevice = getMsb(0,4);
	    *pOBC = cmd;
	    *pHex = cmd;
        strcpy(pProtocol, "DirecTV");

		// Report short/long lead-out based only on first frame, since final frame
		// will always have a long lead-out.  Use of static variabl achieves this.  
		static int parm = (*pFrameEnd > 20000);
		parm &= 1;
		parm |= nFreq > 48000 ? 4 : nFreq > 39000 ? 0 : 2;
		sprintf(pMisc, "Parm = %d", parm);
    }
}  // tryDirecTV

void Signal::tryQ1()
{
	// {38K,226}<2,-2|2,-3|2,-4|2,-5>(4,-2,D:8,F:6,M:2,C:4,~F:4,2,-356)+
	//

	if (nFrameL != 14)
		return;
	if ( sortOn.max2 >= pFrame[0] )
		return;
	if ( ! framed(sortBurst.max1*2) )
		return;
	double unit = (pFrame[0]+pFrame[1])*(1./6);
	if ( sortBurst.max1 > unit*6.6 )
		unit = sortBurst.max1 * (1./7);
	if ( sortBurst.min1 < unit*3.55 )
		return;
	if ( sortOn.max2 > unit * 2.5 || sortOn.max1 > unit * 4.5 )
	{
		return;
	}
	cleanup();
	double mult = 4/unit;
	do
	{
		pBit += 2;
		int x = (int)floor( (pBit[0]+pBit[1])*mult + (2.5-16) );
		if ( ! ( x & 3 ) )
			return;
		cBits[nBit>>3] |= (x>>2) << (nBit & 7);
		nBit += 2;
	
	}   while ( nBit < 24 );

	sprintf(pProtocol, "?1-%02X-%02X-%02X", cBits[0], cBits[1], cBits[2]);

	*pDevice = cBits[0];
	*pSubDevice = getLsb(14,2);
	*pOBC = getLsb(8,6);
	sprintf(pMisc, "C=%X", getLsb(16,4)^getLsb(8,4) );

}

void Signal::tryQ2()
{
	// {38k}<-624,468|468-624>(1820,-590,0:1,D:4,F:7,S:1,C:4,1:1,-???)
	// {38k}<-624,468|468-624>(1820,-590,0:1,D:4,S:6,F:6,C:4,1:1,-???)
	if (   pFrameEnd[0] < 3000.
		|| pFrame[0]    < 1000.
		|| pFrame[0]    > 2000.
		|| pFrame[1]    < 500.
		|| pFrame[1]    > 1300.
		|| sortBurst.max2   > 2300.
		|| sortBurst.max2   > pFrame[0] + pFrame[1]
		|| sortBurst.min1    < 900. )
	{
		return;
	}

	if ( preemptValue >= prQ2 )		// GD 2009  Avoids spurious decode of OrtekMCE
	{
		// the tolerances for OrtekMCE overlap with those for S:17
		// signal, so potentially every OrtekMCE signal could be
		// mistaken for an S:17 one.
		return;
	}

	m_minShortPulse = 200. ;
	m_nominalShortPulse = 468. ;
	m_maxShortPulse = 525. ;
	m_nominalLongPulse = 936. ;
	m_maxLongPulse = 1050. ;

	// GD 2009 Added initializations of two "nominal" values whose omission
	// caused "Invalid floating point operation" in certain circumstances

	m_minShortGap = 500. ;
	m_nominalShortGap = 624. ;  // GD 2009
	m_maxShortGap = 850. ;
	m_minLongGap =  950. ;
	m_nominalLongGap = 1248. ;  // GD 2009
	m_maxLongGap = 1700. ;

	m_extra = 0. ;

   	cleanup();
	++pBit;
	nState = 0;

	do
	{
		if ( phaseBit2() <= 0 )
			return;

	} while ( pBit < pFrameEnd );

	if ( nBit < 10 )
		return;

	if ( (nBit&3)==2 && getBit(0)==1 && getBit(nBit-1)==0 )
	{
		int sum=0;
		char *p1 = pMisc;
		int ndx;
		int dig;
		for (ndx=1; ; )
		{
			dig = 15 - getLsb(ndx,4);
			sprintf( p1, "%X ", dig );
			p1 += 2;
			ndx += 4;
			if ( ndx >= nBit-1 )
				break;
			sum = (sum>>4) + (sum&15) + dig;
		}
		if ( (sum&=15) != dig )
		{
			sprintf(p1-1, "-%X", sum);
		}
		else
		{
			sprintf( pProtocol, "Solidtek%d", nBit-2 );
			if ( nBit == 22 )
			{
				*pDevice = 15-getLsb(1,4);
				*pSubDevice = 63-getLsb(5,6);
				*pOBC = 63-getLsb(11,6);
				return;
			}
			if ( nBit == 18 )
			{
				*pDevice = 15-getLsb(1,4);
				*pSubDevice = 1-getLsb(12,1);
				*pOBC = 127-getLsb(5,7);
				if (	*pDevice == 0
					&&	*pSubDevice == 0
					&&	*pOBC == 0 )
				{	
					newPreemptValue = prBySolidtek; 	// Avoid RC6-8-13 decode of same frame
					newPreemptLength = nFrameL;
				}
			}
			return;
		}
	}
	char* p1 = pProtocol;
	sprintf( p1, "S:%d", nBit );
	unsigned char* pb = cBits;
	pb[nBit>>3] += 0xFF << (nBit&7);
	do
	{
		sprintf( p1+=strlen(p1), ".%02X", msb(255-*pb) );
		++pb;
	} while ( (nBit-=8) > 0 );


}

#ifdef _MSC_VER
BOOL APIENTRY DllMain( HANDLE hModule, 
					   DWORD  ul_reason_for_call, 
					   LPVOID lpReserved
					 )
{
	switch (ul_reason_for_call)
	{
		case DLL_PROCESS_ATTACH:
		case DLL_THREAD_ATTACH:
		case DLL_THREAD_DETACH:
		case DLL_PROCESS_DETACH:
			break;
	}
	return TRUE;
}
#endif


const char *(Protocols[]) =
{
	"Acer",
	"Airboard",
	"Akai",
	"Amino",
	"Anthem",
	"CanalSat",
	"Denon",
	"Dishplayer",
	"Dgtec",
	"Emerson",
	"F12",
	"Feelux",
	"G.I.4DTV",
	"G.I. Cable",
	"Grundig",
	"GXB",
	"IODATA",
	"Jerrold",
	"JVC",
	"Kathrein",
	"Konka",
	"Lumagen",
	"NEC",
	"NEC1",
	"NEC2",
	"Old Panasonic",
	"OrtekMCE",
	"Panasonic",
	"Pioneer",
	"Proton",
	"RC6",
	"RC5",
	"RC5x",
	"RCA",
	"Replay",
	"Russsound",
	"S16",
	"S42",
	"Sampo",
	"Samsung",
	"ScAtl-6",
	"Sejin",
	"Sharp",
	"Somfy",
	"SONY",
	"Sunfire",
	"TDC",
	"XMP",
	"Zenith5",
	"Zenith6",
	"Zenith7"
};

extern "C" {

//This function takes a protocol number (1 based, and returns the protocol name 
//to the buffer provided by the calling application
void _stdcall EnumerateProtocols(int iProtocolNumber, char* TsProtocol)
{
	if (iProtocolNumber < sizeof(Protocols)/sizeof(*Protocols) )
	{
		strcpy(TsProtocol, Protocols[iProtocolNumber]);
	}
	else
		strcpy(TsProtocol,"");
	
};

//This function takes a protocol name and returns the support level of the protocol, 0 = not supported
int _stdcall ProtocolSupportLevel(char* TsProtocol)				
{

	for (int ndx=sizeof(Protocols)/sizeof(*Protocols); --ndx>=0; )
		if ( strnicmp( Protocols[ndx], TsProtocol, strlen(Protocols[ndx]) ) == 0 )
			return 1;
	return 0;

};

void _stdcall Version(char *Result)
{
	strcpy( Result, version_cstr );
}

void DecodeIR_API DecodeIR
(	unsigned int* Context,		//In/Out - Array of unsigned 4 byte integers.  For input it tells dll in what context it should 
								//decode the signal, and on output the context it would need to decode the next part of the signal.
								//It has a minimum length of 2, and on first call, both integers are 0.  Further information can
								//be passed in either direction by increasing its length.  See TiSubDevice below.
	int* TpaiBursts, 			//In - Array of integers containing the burst lengths in microseconds, 
								//the length of the array is 2*(TiSingleBurstCount + TiRepeatBurstCount +
								//ExtraBurstCount), where ExtraBurstCount is carried in TiDevice, see below.
	int TiFreq,					//In - Frequency in Hz of the signal.  Use -1 for unknown.  Any positive
								//   value less than 500000/longest_on will be treated as unmodulated, but
								//   normally unmodulated should be represented as 0.
	int TiSingleBurstCount,		//In - Number of bursts sent once
	int TiRepeatBurstCount,		//In - Number of bursts sent repeatedly

	// For all int* outputs, the caller must preset the value to -1 and the decoder will not change the value
	//   unless it finds a decode and that decode includes that value.
	// Similarly char* outputs should have their first byte set to 0 by the caller and the decoder will
	//   overwrite with a null terminated string only when that string is not empty.
	char* TsProtocol,			//Out - 255 character string name of the decoded protocol
	int* TiDevice,				//Out - Integer device number.
								//In  - If < -1 and RepeatBurstCount > 0, it is -ExtraBurstCount.  If < -1
								//and RepeatBurstCount = 0 it flags that the frame count should not be returned
								//to the caller.  The caller should include any extra bursts in with the single
								//bursts in this case.  All versions of IR.exe have done this.
	int* TiSubDevice,			//Out - Integer sub device number.
								//In  - If < -1 it is negative of the length of the Context array.  A handshake
								//protocol is used to return to the caller the actual length that has been
								//interpreted and acted upon by the version of DecodeIR in use.  This version
								//supports a maximum length of 9.  See decode2() for more details.
	int* TiOBC,					//Out - Integer OBC of the decoded signal
	int* TaiHex,				//Out - Array of 4 integers containing up to 4 different one-byte Hex commands
								//if returning less that 4 hex values, the remaining integers should be -1
	char* TsMisc,				//Out - 255 character string of Misc text, used whenever more explanation of 
								//the results should be returned
	char* TsError)				//Out - 255 character string containing an error message if the dll encountered an error
{
	if (TiSingleBurstCount + TiRepeatBurstCount >= MinFrame)
	{
		char szMessage[500];
		sprintf(szMessage,"N=%d SCount=%d RCount=%d Burst1=%d Freq=%d hex[0]=%d Error=\"%s\""
			,*Context, TiSingleBurstCount, TiRepeatBurstCount, TpaiBursts[1],TiFreq,TaiHex[-1],TsError);
		//	MessageBox(0,szMessage,"DecodeIR",0);
		Signal _Signal(
			Context,
			TpaiBursts, 	
			TiFreq, 
			TiSingleBurstCount, 
			TiRepeatBurstCount, 
			TsProtocol, 
			TiDevice, 
			TiSubDevice, 
			TiOBC, 
			TaiHex, 
			TsMisc, 
			TsError );

		_Signal.decode2();	// GD 2009 changed from decode()
	}
	return;
}
}

int raw_to_pronto(int* argc_ptr, char** argv_ptr[]) {
  char *p1 = (*argv_ptr)[1];
  if (!p1)
    return 0;

  char *st = strdup(p1);
  char *ch = strtok(st, ",");
  if (strcmp(ch,p1)==0) {
    free(st);
    return 0;
  }

  const int max_size = 1024;
  static char* argv[max_size];
  argv[0] = strdup((*argv_ptr)[0]);

  char buf[8];
  int size = 5;
  for (;ch && size<max_size; size++) {
    int value = abs(atoi(ch));
    sprintf(buf,"%04X", value*38000/1000000);
    argv[size] = strdup(buf);
    ch = strtok(NULL, ",");
  }

  free(st);

  argv[1] = strdup("0000");
  argv[2] = strdup("006D");
  sprintf(buf,"%04X", (size-4)/2);
  argv[3] = strdup(buf);
  argv[4] = strdup(argv[1]);

  *argv_ptr = argv;
  *argc_ptr = size;

  return 1;
}

int main(int argc, char* argv[]) 
{
  unsigned int i = 1; 
  int type; 

  raw_to_pronto(&argc, &argv);
  sscanf(argv[i++], "%x", &type); 
  if (type != 0) 
{
    std::cerr << "Can only handle type 0000\n"; 
    exit(1); 
    }
  int fcode; 
  sscanf(argv[i++], "%x", &fcode); 
  int frequency = (int) (1000000.0/((double)fcode * 0.241246)); 
  int intro_length, rep_length; 
  sscanf(argv[i++], "%x", &intro_length); 
  sscanf(argv[i++], "%x", &rep_length); 

  int *data = new int[2*(intro_length+rep_length)]; 
  for (int j = 0; j < 2*(intro_length+rep_length); j++) 
    {
    int ncycles; 
    sscanf(argv[i++], "%x", &ncycles); 
    data[j] = (int)(1000000.0/frequency*ncycles); 
    }
  unsigned int decodeir_context[2] = { 0, 0}; 
  char protocol[255] = ""; 
  int device = -1; 
  int subdevice = -1; 
  int obc = -1; 
  int hex[4] = { -1, -1, -1, -1}; 
  char misc_message[255] = ""; 
  char error_message[255] = ""; 
    
  do 
    {
    DecodeIR(decodeir_context, data, frequency, intro_length, rep_length, 
           protocol, &device, &subdevice, &obc, hex, misc_message, 
           error_message); 

    if (protocol[0] != '\0') 
      std::cout 
      << "protocol=" << protocol 
      << " device=" << device 
      << " subdevice=" << subdevice 
      << " obc=" << obc 
      << " hex0=" << hex[0] 
      << " hex1=" << hex[1] 
      << " hex2=" << hex[2] 
      << " hex3=" << hex[3] 
      << " misc=" << misc_message 
      << " error=" << error_message << "\n"; 
    }
  while (protocol[0] != '\0'); 
}

