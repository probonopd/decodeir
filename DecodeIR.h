
#define DecodeIR_API
#define _stdcall

#define require(x) if (!(x)) { return; }

/*
const CString m_Notes[] = {
	"One frame.",
	"%i identical frames.",
	"%i frames with lead-in on only the first frame.",
	"Main frame with %i ditto frames.",
	"%i frames including distinctive start frame.",
	"%i frames including distinctive end frame.",
	"%i frames including distinctive start and end frames.",
	"%i main frames interleaved with auxiliary check frames.",
	"A generic decode of an unknown protocol",
	"One frame with lead-in.",
	"Main frame with one ditto frame.",
	"%i frames with extended lead-in on the first frame.",
	"Unknown protocol."};

const CString m_AuxNotes[] = {
	"  Incomplete signal: expected end frame missing.",
	"  Incomplete signal: expected start frame missing.",
	"  Incomplete signal: expected start and end frames missing.",
	"  Incomplete signal: main frame without check frame.",
	"  Incomplete signal: check frame without preceding main frame.",
	"  Incomplete signal: start frame with no following frames.",
	"  Incomplete signal: end frame with no preceding frames."};
*/

extern "C" {
void DecodeIR_API DecodeIR
(	unsigned int* Context,
	int* TpaiBursts, 	
	int TiFreq, 
    int TiSingleBurstCount, 
    int TiRepeatBurstCount, 
    char* TsProtocol, 
	int* TiDevice, 
    int* TiSubDevice, 
    int* TiOBC, 
	int TaiHex[4], 
    char* TsMisc, 
    char* TsError);

int _stdcall ProtocolSupportLevel(char * TsProtocol);

void _stdcall EnumerateProtocols(int iProtocolNumber, char* TsProtocol);

void _stdcall Version(char *Result);
};

using namespace std;

typedef float* burst;

struct onLess
{
    int operator()(burst a, burst b)
    {
        if (a[0] != b[0])
            return ( a[0] < b[0] );
        if (a[1] != b[1])
            return ( a[1] < b[1] );
        return a < b;
    }
};

struct offLess
{
    int operator()(burst a, const burst b)
    {
        if (a[1] != b[1])
            return ( a[1] < b[1] );
        if (a[0] != b[0])
            return ( a[0] < b[0] );
        return a < b;
    }
};

struct totLess
{
    int operator()(burst a, burst b)
    {
        float d = a[0]+a[1] - (b[0]+b[1]);
        if ( d > 0.0 )
            return 0;
        if ( d < 0.0 )
            return 1;
        if (a[0] != b[0])
            return ( a[0] < b[0] );
        return a < b;
    }
};

struct setBurst : public set<float>
{
    typedef set<float> super;
    float min3;
    float min2;
    float min1;
    float mid1;
    float mid2;
    float max1;
    float max2;
    float max3;

    setBurst()
    {
        clearF();
    }
    void clear()
    {
        super::clear();
        clearF();
    }
    void clearF()
    {
        min3 = 1e9;
        min2 = 1e9;
        min1 = 1e9;
        max1 = 0;
        max2 = 0;
        max3 = 0;
    }
    void insert1( burst b )
    {
        double x = b[0]+b[1];
        super::insert( x );
        if ( x < min3 )
        {
            if ( x >= min2 )
            {
                min3 = x;
            }
            else
            {
                min3 = min2;
                if ( x >= min1 )
                {
                    min2 = x;
                }
                else
                {
                    min2 = min1;
                    min1 = x;
                }
            }
        }
        if ( x > max3 )
        {
            if ( x <= max2 )
            {
                max3 = x;
            }
            else
            {
                max3 = max2;
                if ( x <= max1 )
                {
                    max2 = x;
                }
                else
                {
                    max2 = max1;
                    max1 = x;
                }
            }
        }
    }
    void insert( burst b )
    {
        double x = b[0]+b[1];
        super::insert( x );
        bool redo = ( x > mid1 && x < mid2 );
        if ( x < min3 )
        {
            if ( x >= min2 )
            {
                min3 = x;
            }
            else
            {
                min3 = min2;
                double g = min1 - x;
                if ( g <= 0 )
                {
                    min2 = x;
                }
                else
                {
                    min2 = min1;
                    min1 = x;
                    if ( g > mid2-mid1 ) redo = true;
                }
            }
        }
        if ( x > max3 )
        {
            if ( x <= max2 )
            {
                max3 = x;
            }
            else
            {
                max3 = max2;
                double g = x-max1;
                if ( g <= 0 )
                {
                    max2 = x;
                }
                else
                {
                    max2 = max1;
                    max1 = x;
                    if ( g > mid2-mid1 ) redo = true;
                }
            }
        }
        if (redo)
        {
            findMid();
        }
    }
    void findMid()
    {
        iterator it = begin();
        iterator limit = end();
        float x = (*it);
        mid1 = x;
        mid2 = x;
        float m = 0;
        while ( ++it != limit )
        {
            float y = (*it);
            float z = y-x;
            if ( z > m )
            {
                m = z;
                mid1 = x;
                mid2 = y;
            }
            x = y;
        }
    }

};

struct setOn
{
    float min1;
    float max1;
    float max2;
    setOn()
    {
        clear();
    }
    void clear()
    {
        min1 = 1e9;
        max1 = 0;
        max2 = 0;
    }
    void insert( burst b )
    {
        double x = b[0];
        if ( x < min1 ) min1 = x;
        if ( x > max2 )
        {
            if ( x > max1 )
            {
                max2 = max1;
                max1 = x;
                return;
            }
            max2 = x;
        }
    }
};

struct setOff
{
    float min1;
    float max1;
    setOff()
    {
        clear();
    }
    void clear()
    {
        min1 = 1e9;
        max1 = 0;
    }
    void insert( burst b )
    {
        double x = b[0];
        if ( x < min1 ) min1 = x;
        if ( x > max1 ) max1 = x;
    }
};

enum { MinFrame=2, MaxFrame=100 };

struct Signal
{
	Signal(
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
		char* p_Error);
	~Signal();

	void decode2();		// GD 2009
	void decode();
	void setPreempt(int prValue);	// GD 2009
	void setzContext();				// GD 2009
	unsigned int getFreq(int start, int end);	// GD 2009
	void cleanup();
	int phaseBit();
	int phaseBit2();
	int framed(float nGap);
	int framedLeft(float nGap);
	int unmodulated(float maxOn) {return nFreq*maxOn < 500000.;}
	unsigned int getMsb(int first, int bits);
	unsigned int getLsb(int first, int bits);
    int getBit(int n) { return cBits[n>>3]&(1<<(n&7)); }
	int msb(int val, int bits);
	void makeMsb();
	void decodeX( int nCount );
	void decodeX2( int nCount );
    int checkDecodeX( int start, int count, float minShort, float maxLong, float maxFront );
	int decodeRaw( int bitsRequested );
	int decodeAsync( float *pData, int bits, int sizes, double dBitMin, double dBitMax, int bitsPerByte, int minBits );
	int lead_in(float minTot, float maxTot, float maxOn, float minOff, float minFrameInc)
	{
        if (pLead)
        {
            if ( pLead[1] >= minOff && pLead[0] <= maxOn )
            {
                float t = pLead[0] + pLead[1];
                if ( t >= minTot && t <= maxTot && nFrame1 >= t+minFrameInc )
                    return 1;
            }
            if (pLead2)
            {
                if ( pLead2[1] >= minOff && pLead2[0] <= maxOn )
                {
                    float t = pLead2[0] + pLead2[1];
                    if ( t >= minTot && t <= maxTot && nFrame2 >= t+minFrameInc )
                        return 1;
                }
            }
        }
        return 0;
	}
	bool processHumaxAtoms(int bitStart, float* pFr, int maxBursts); 
	bool processManchesterAtoms(int burstStart, int numBits, float used, float bitDur, float* pFr); 
	void tryRC6();
	void tryRC5();
	void tryNokia();
	void tryGap();
	void tryZenith();
    void tryXMP();
    void trySomfy();
	void tryX10();
	void tryPCTV();
	void tryPid13();
	void tryF12();
	void trySingleBurstSize();
	void trySony();
	void tryGXB();
	void tryAsync();
	void tryAirboard();
	void tryAirAsync();
	void tryAK();
	void tryXX();
	void tryQ1();
	void tryQ2();
    void tryDirecTV();
    void trySunfire();
    void tryGrundig16();
	void tryTDC();			// GD 2009
	void tryCanalSat();		// GD 2009
	void trySejin();		// GD 2009
	void tryLutron();		// GD 2010
	void tryElan();			// DAR 2012
	void tryBryston();		// DAR 2012
	void tryHumax();		// DAR 2012
	void tryAdNotam();		// DAR 2012
	
    int moreBlaupunkt(int bits);

	unsigned int* pContext;
	float* pDuration;
    float* pMainLimit;
    float* pFullLimit;
	int nSingle;        // Bursts in single part
	int nRepeat;        // Bursts in repeat part
	int nExtra;			// GD 2009 Bursts in extra part
	int nFreq;
	int nFreq_in;		// GD 2009 Input freq (actual freq may be calculated)
	float* pFrame;
	float* pFrameEnd;
	int nFrameL;        // Burst count of frame 
	int nProtocol;
	int preemptLength;
    int preemptValue;
    int newPreemptLength;
    int newPreemptValue;
	// GD 2009 start
	int nFrameCount;
	int bSuppressCount;
	int nDittos;
	int nNonSpurious;	// Count of assuredly non-spurious bursts 
	int bInitLeadIn;	// Set when a single lead-in precedes a repeat sequence, eg JVC protocol
	int nContextLength;	// Length of Context[], default 2
	int nGapProtocol;	// Index of tryGap in Funcs[] (only protocol to use pLead)
	int bXMPHalfValid;	// Boolean set when an incomplete XMP decode has first half frame with valid checksum
	unsigned int zContext[2];	// Saved context value from start of repeat series

	// The values nNote_out and nAuxNote_out are indexes (base 0) into arrays m_Notes and m_AuxNotes 
	// of notes defined inIRScope.  A copy of those definitions is given above.  A value -1 means
	// no note is assigned.

	int nNote_out;		// Ref number of note for return to IRScope
	int nAuxNote_out;	// Ref number of aux note for return to IRScope
	short int* pCounts;	// Provided in pContext[4] by IRScope for calc of freq for individual decodes
	// GD 2009 end
	float nTotDur;  	// Excluding Off half of leadOut
	float nMaxDur;  	// Excluding Off half of leadOut

	unsigned char cBits[16];
	char* pSuffix;		//DAR Dec 2010 String which may be appended to pProtocol
	int AminoToggleDetected;	// DAR July 2011  Set when a correct toggle sequence is found
	int ZaptorToggleDetected;
	int nBit;
	int nState;
	float* pBit;
	float* pLead;
	float* pLead2;

    float minGlitch;
    float nMinShort;
	float nMaxShort;
	float nMinLong;
	float nMaxLong;
	float nMinShort2;
	float nMaxShort2;

    union
    {
        struct
        {
            float m_minShortPulse;
            float m_nominalShortPulse;
            float m_maxShortPulse;
            float m_nominalLongPulse;
            float m_maxLongPulse;

            float m_minShortGap;
            float m_maxShortGap;
            float m_minLongGap;
            float m_maxLongGap;

            float m_nominalShortGap;
            float m_nominalLongGap;
            float m_negDelta;   // vs. nominal full bit time
            float m_posDelta;
        };
        struct
        {
            float m_rawUnit;
            float m_rawPulseAdjust;
            float m_rawGapAdjust;
            float m_rawErrorLimit;
        };
    };

    float m_extra;

    float nFrame1;
    float nFrame2;

    float frameLeft;
    float frame;

	char* pProtocol;
	char* pMisc;
	char* pError;
	int* pDevice;
	int* pSubDevice;
	int* pOBC;
	int* pHex;

    setOn sortOn;
    setOff sortOff;
    setBurst sortBurst;
};  // struct Signal

inline unsigned int parity(unsigned int p)
{
    unsigned int x = p ^ (p>>16);
    x = x ^ (x >> 8);
    x = x ^ (x >> 4);
    x = x ^ (x >> 2);
    return ( x ^ (x >> 1) ) & 1;
}

enum
{
    prGap = 1,
    prAsync = 1,
    prByXMP = 1,
    prByZenith = 1,
    prByPid_0003 = 1,
    prByDenon = 1,				// GD 2009 Denon protocol 2nd half
    prBySamsung36 = 1,
    prByBlaupunkt = 1,
    prBlaupunkt = prAsync + 1,
    prByPid_0001 = prBlaupunkt,
    prBySony = prBlaupunkt,
    prJerrold = prBlaupunkt,
	prByGI_RG = prBlaupunkt,	// DAR 2012
	prBySIM2 = prBlaupunkt,		// DAR 2012
	prQ2 = prBlaupunkt + 1,		// GD 2009
	prRCx = prQ2,				// GD 2009
	prByOrtekMCE = prQ2,		// GD 2009
	prBySolidtek = prQ2			// GD 2009
	
};

inline int Signal::framedLeft(float nGap)
{
	return nGap <= frameLeft;
}

inline int Signal::framed(float nGap)
{
	return nGap <= frame;
}
