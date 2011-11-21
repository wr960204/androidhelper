Unit ztvConsts;

Interface


Uses
	Windows, Classes, SysUtils;


Type
	ztv_WString = AnsiString; 
   _uint 		= uInt; //Integer; // requires compiler option $R- (range checking off)
  	u_long  		= Cardinal;
   fs_long 		= Cardinal;
  	Int32   		= Longint;
   pWord       = ^Word;
  	Word16  		= Word;
  	Word32  		= LongWord;
  	Word64  		= Int64;
  	pWord64 		= ^Int64;
  	uIntf  		= _uInt;		// v6.4.3 added underscore... windows also defines a uInt variable
  	ush  			= Word;
  	uch  			= Byte;
  	ushf 			= ush;
  	_unsigned 	= _uInt;
  	ulg  			= LongInt;
  	ptr2int 		= _uInt;
   pBoolean 	= ^Boolean;
  	_Bytef  		= byte;
  	_pBytef 		= ^_Bytef;
  	charf  		= byte;
  	pcharf 		= ^charf;
  	_Pos 			= ush;
  	Posf 			= _Pos;
  	pPosf 		= ^Posf;
 	IPos 			= _uInt;
  	_int    		= integer;
  	intf   		= _int;
  	Long   		= longint;
  	uLongf 		= u_long;
  	voidp  		= pointer;
  	voidpf 		= voidp;
  	pIntf  		= ^intf;
  	puIntf 		= ^uIntf;
  	puLong 		= ^uLongf;

   
Const
	MAX_MEM_LEVEL = 9;
   DEF_MEM_LEVEL = 8;
  	MaxMemBlock = MaxInt;


Type
   THeaderStatus = (hsAdd, hsCopy, hsSkip, hsRename);
   TztvSortOptions = (soUnsort, soByName, soByType, soBySize, soByTime, soByPath);

   TztvFileAttribute = (fsZeroAttr, fsReadOnly, fsHidden, fsSysFile,
   	fsVolumeID, fsDirectory, fsArchive, fsCompressed, fsEncrypted);

   TztvFileAttributes = Set Of TztvFileAttribute;

   TPartType =
   	(pt1mb, ptFloppy_120mb, ptFloppy_144mb, ptFloppy_288mb,
      ptZipDisk_100mb, ptZipDisk_250mb, ptCDRom_650mb, ptCDRom_700mb,
      ptOtherSize, ptNoSplit);

   TCustomSizeType = (stBytes, stKB, stMB);
   TDeleteOptions = (doFinal, doAllowUndo);
   TCompressionMethod = (cmInMemory, cmTempFile);


Type
   _alloc_func = Function(opaque: voidpf; Items, Size: u_long): voidpf;
   _free_func = Procedure(opaque: voidpf; address: voidpf);
   _check_func = Function(Check: u_long; Buf: _pBytef; Len: uInt): u_long;

   TOnDiskError = Procedure(Sender: TObject; Var Abort: Boolean)
      Of Object;

Type
   zuIntArray = Array[0..(MaxMemBlock Div SizeOf(uInt)) - 1] Of {u_long; //}_uInt; //v6.1
   PuIntArray = ^zuIntArray;

   inflate_block_mode =
      (ZTYPE,                           { get type bits (3, including end bit) }
      LENS,                             { get lengths for stored }
      STORED,                           { processing stored block }
      TABLE,                            { get table lengths }
      BTREE,                            { get bit lengths tree for a dynamic block }
      dtree,                            { get length, distance trees for a dynamic block }
      CODES,                            { processing fixed or dynamic block }
      DRY,                              { output remaining window bytes }
      BLKDONE,                          { finished last block, done }
      BLKBAD);                          { got a data error--stuck here }


Type
   pphuft = ^phuft;
   phuft = ^huft;
   //phuftlist = ^HuftList;
   huft = Record
      e,                             	 { (*exop*) number of extra bits or operation }
      b: Byte;                          { (*bits*) number of bits in this code or subcode }
      //pad : uInt;                     {          pad structure to a power of 2 (4 bytes for }
                                        {          16-bit, 8 bytes for 32-bit int's) }
      v_n: ush;								 { (*base*) literal, length base, or distance base }
      //v_t: phuftlist;                 { Linked List }
   End;
   //HuftList = Array[0..8190] Of huft;

   inflate_codes_mode = ( { waiting for "i:"=input, "o:"=output, "x:"=nothing }
      Start,                            { x: set up for LEN }
      Len,                              { i: get length/literal/eob next }
      LENEXT,                           { i: getting length extra (have base) }
      dist,                             { i: get distance next }
      DISTEXT,                          { i: getting distance extra }
      zCOPY,                            { o: copying bytes in window, waiting for space }
      lit,                              { o: got literal, waiting for output space }
      WASH,                             { o: got eob, possibly still output waiting }
      zend,                             { x: got eob and all data flushed }
      BADCODE);                         { x: got error }

   pInflate_codes_state = ^inflate_codes_state;
   inflate_codes_state = Record
      Mode: inflate_codes_mode;
      Len: uInt;
      sub: Record                       { submode }
         Case Byte Of
            0: (code: Record            { if LEN or DIST, where in tree }
                  tree: phuft;          { pointer into tree }
                  need: uInt;           { bits needed }
               End);
            1: (lit: uInt);             { if LIT, literal }
            2: (Copy: Record            { if EXT or COPY, where and how much }
                  get: uInt;            { bits to get for extra }
                  dist: uInt;           { distance back to copy from }
               End);
      End;

      lbits: Byte;                      { ltree bits decoded per branch }
      dbits: Byte;                      { dtree bits decoder per branch }
      ltree: phuft;                     { literal/length/eob tree }
      dtree: phuft;                     { distance tree }
   End;

   huft_field = Array[0..(MaxMemBlock Div SizeOf(huft)) - 1] Of huft;
   huft_ptr = ^huft_field;

   pInflate_blocks_state = ^inflate_blocks_state;
   inflate_blocks_state =
      Record
      Mode: inflate_block_mode;
      sub:
      Record
         Case Byte Of
            0: (left: uInt);            { if STORED, bytes left to copy }
            1: (trees: Record           { if DTREE, decoding info for trees }
                  TABLE: {u_long; //}_uInt; //v6.1          { table lengths (14 bits) }
                  Index: {u_long; //}_uInt; //v6.1         { Index into blens (or border) }
                  blens: PuIntArray;    { bit lengths of codes }
                  bb: {u_long; //}_uInt;  //v6.1           { bit length tree depth }
                  tb: phuft;            { bit length decoding tree }
               End);
            2: (decode: Record          { if CODES, current state }
                  tl: phuft;
                  td: phuft;            { trees to free }
                  CODES: pInflate_codes_state;
               End);
      End;
      last: Boolean;                    { true if this block is the last block }
      bitk: {u_long; //}_uInt; //v6.1        { bits in bit buffer }
      bitb: u_long;                     { bit buffer }
      hufts: huft_ptr;                  { single malloc for tree space }
      window: _pBytef;                  { sliding window }
      zend: _pBytef;                    { one byte after sliding window }
      Read: _pBytef;                    { window read pointer }
      Write: _pBytef;                   { window write pointer }
      CheckFn: _check_func;             { check function }
      Check: u_long;                    { check on output }
   End;

   inflate_mode = (
      method,                           { waiting for method byte }
      FLAG,                             { waiting for flag byte }
      DICT4,                            { four dictionary check bytes to go }
      DICT3,                            { three dictionary check bytes to go }
      DICT2,                            { two dictionary check bytes to go }
      DICT1,                            { one dictionary check byte to go }
      DICT0,                            { waiting for inflateSetDictionary }
      BLOCKS,                           { decompressing blocks }
      CHECK4,                           { four check bytes to go }
      CHECK3,                           { three check bytes to go }
      CHECK2,                           { two check bytes to go }
      CHECK1,                           { one check byte to go }
      DONE,                             { finished check, done }
      zBAD);                            { got an error--stay here }

   pInternal_state = ^internal_state;
   internal_state =
      Record
      Mode: inflate_mode;
      sub:
      Record                            { submode }
         Case Byte Of
            0: (method: uInt);          { if FLAGS, method byte }
            1: (Check: Record           { if CHECK, check values to compare }
                  was: u_long;          { computed check value }
                  need: u_long;         { stream check value }
               End);
            2: (marker: uInt);          { if BAD, inflateSync's marker bytes count }
      End;
      nowrap: Boolean;                  { flag for no wrapper }
      wbits: uInt;                      { log2(window Size)  (8..15, defaults to 15) }
      BLOCKS: pInflate_blocks_state;    { current inflate_blocks state }
   End;

   ztv_stream_plus = Packed Record
      CRC: u_long;
      Protect: Boolean;
      pArchivePos: ^Int64;
      pCancel: pBoolean;
      IsEof: Boolean;
   End;

   ztv_streamp = ^ztv_stream;
   ztv_stream = Packed Record
      next_in: _pBytef;                 { next input byte }
      avail_in: Longint; //uInt;        { number of bytes available at next_in }
      total_in: Int64; //u_long;        { total nb of input bytes read so far }
      next_out: _pBytef;                { next output byte should be put there }
      avail_out: uInt;                  { remaining free space at next_out }
      total_out: Int64; //u_long;       { total nb of bytes output so far }
      //msg: String[255];                 { last error message, '' if no error }
      state: pInternal_state;           { not visible by applications }
      ZALLOC: _alloc_func;              { used to allocate the internal state }
      ZFREE: _free_func;                { used to free the internal state }
      opaque: voidpf;                   { private data object passed to zalloc and zfree }
      data_type: _int; { best guess about the data type: ascii or binary }
      adler: u_long;                    { adler32 value of the uncompressed data }
      Reserved: u_long;                 { reserved for future use }
      cb: ztv_stream_plus;
   End;

Const
	Z_BINARY    			= 0;
   Z_ASCII     			= 1;
   Z_UNKNOWN   			= 2;
   Z_DEFLATED  			= 8;
   Z_DEFLATE64 			= 9;
   Z_NULL     				= NIL;

   Z_FILTERED           = 1;
   Z_HUFFMAN_ONLY       = 2;
   Z_DEFAULT_STRATEGY   = 0;

  	Z_NO_FLUSH      		= 0;
  	Z_PARTIAL_FLUSH 		= 1;
  	Z_SYNC_FLUSH    		= 2;
  	Z_FULL_FLUSH    		= 3;
  	Z_FINISH        		= 4;

Const
   unzip_Ok 				=  0;
   unzip_WriteErr 		= -2;
   unzip_ReadErr 			= -3;
   unzip_ZipFileErr 		= -4;
   unzip_UserAbort 		= -5;

   // return codes
  	Z_OK            		= 0;             		{ OK             $00000001}
  	Z_STREAM_END    		= 1;             		{ STREAM_END     $00000002}
  	Z_NEED_DICT     		= 2;             		{ NEED_DICT      $00000004}
  	Z_ERRNO         		= (-1);          		{ ERRNO          $00000010}
  	Z_STREAM_ERROR  		= (-2);          		{ STREAM_ERROR   $00000020}
  	Z_DATA_ERROR    		= (-3);          		{ DATA_ERROR     $00000040}
  	Z_MEM_ERROR     		= (-4);          		{ MEM_ERROR      $00000080}
  	Z_BUF_ERROR     		= (-5);          		{ BUF_ERROR      $00000100}
  	Z_VERSION_ERROR 		= (-6);          		{ VERSION_ERROR  $00000200}

   BASE 						= u_long( 65521 );   { largest prime smaller than 65536 }
   NMAX 						= 3854;              { code with signed 32 bit _int }

   z_errbase 				= Z_NEED_DICT;
   INIT_STATE 				= 42;
   BUSY_STATE 				= 113;
   FINISH_STATE 			= 666;
   _MAX_WBITS 				= -15;              	{ 32K LZ77 window }
  	RAND_HEAD_LEN 			= 12;               	{ length of encryption Random header }
   CRC_MASK: u_long 		= $FFFFFFFF;

Const
   // GZip CompressType flags
   GZIP_MAGIC = $8B1F;                  // 35615
   OLD_GZIP_MAGIC = $9E1F;              // 40479
   LZW_MAGIC = $9D1F;                   // 40223
   LZH_MAGIC = $A01F;                   // 40991
   PACK_MAGIC = $1E1F;                  // 7711
   RootDir = $5C;                       // 92


   (* Header Signatures *)
Const
   LHA_SIGNATURE = $6C2D;
   ZOO_SIGNATURE = $4F5A;               //dec value = 20314   79/90
   ARJ_SIGNATURE = $EA60;
   DCP_SIGNATURE = $35474B50;

   MS_GZ_HEADER_SIGNATURE = Integer(-1939514093);
   BLAKHOLE_SIGNATURE = $7054842;
   MAIN_RAR_HEADER_SIGNATURE = $21726152;
   //ZLIB_HEADER_SIGNATURE = DCP_SIGNATURE;

   LOCAL_FILE_HEADER_SIGNATURE = $04034B50;
   LOCAL_FILE_ENCRPT_SIGNATURE = $04034C50;
   LOCAL_CUST_HEADER_SIGNATURE = $04034D50;
   LOCAL_CUST_ENCRPT_SIGNATURE = $04034E50;

   CENTRAL_FILE_HEADER_SIGNATURE = $02014B50;
   CENTRAL_FILE_ENCRPT_SIGNATURE = $02014C50;
   CENTRAL_CUST_HEADER_SIGNATURE = $02014D50;
   CENTRAL_CUST_ENCRPT_SIGNATURE = $02014E50;

   CENTRAL_FILE_HEADER_DIGITAL = $05054b50;

   END_OF_CENTRAL_HEADER_SIGNATURE = $06054B50;
   END_OF_CENTRAL_ENCRPT_SIGNATURE = $06054C50;
   END_OF_CENTRAL64_HEADER_SIGNATURE = $06055B50;
   END_OF_CENTRAL64_ENCRPT_SIGNATURE = $06055C50;

   END_OF_CENTRAL_WZIP_HEADER_SIGNATURE = $06064B50;
   CENTRAL_WZIP_HEADER_LOCATOR = $07064b50;
   MULTIVOL_HEADER_SIGNATURE = $08074B50;

   CAB_SIGNATURE = $4643534D;
   CAB_SIGNATURE64 = $3e0c140048d104d;
   GZIP_HEADER_SIGNATURE = GZIP_MAGIC;
   ZIPTV_SFX_SIGNATURE = $5846535A;

Const
	SIXTYFOUR_BIT_HDR_ID_WZIP = $001;
	SIXTYFOUR_BIT_HDR_ID_ZIPTV = $0064;

   LDISK = $00;
   FDISK = $01;
   NDISK = $02;

   Z_NO_COMPRESSION = 0;
   Z_BEST_SPEED = 1;
   Z_DEFAULT_COMPRESSION = (-1);
   Z_BEST_COMPRESSION = 9;

Type
   TDeflateType =
      (dtDeflateS, dtDeflateF, dtDeflateN, dtDeflateX);

Const
   ZLevels: Array[TDeflateType] Of ShortInt = (
      Z_NO_COMPRESSION,
      Z_BEST_SPEED,
      Z_DEFAULT_COMPRESSION,
      Z_BEST_COMPRESSION
      );

   // Compression Methods
Const
   ZTV_STORED = 0;
   ztv_FUSE = 1;
   ZTV_FUSE6 = 3;
   ztv_GREATEST = 2;
   ztv_DEFLATE = 8;
   ztv_DEFLATE64 = 9;
   ztv_FROZEN0 = 48;
   ztv_FROZEN1 = 49;
   ztv_FROZEN5 = 53;
   ztv_FROZEN6 = 54;
   ztv_FROZEN7 = 55;

   // Deflate pack levels
Const
   ztvDeflateS = 1;
   ztvDeflateF = 2;
   ztvDeflateN = 6;                     //7; v4.6.8 revised
   ztvDeflateX = 9;

   // Header Sizes
Const
   GZipHdr_Size = 10;
   AceMainHeader_Size = 31;
   HAHdr_Size = 18;
   ZooHdr_Size = 43;
   ZooDirHdr_Size = 58;
   RAR1Hdr_Size = 13;
   //RARHdr_Size = 32;
   LZHHdr_Size = 22;
   ArcHdr_Size = 29;
   PakHdr_Size = 29;
   ArjHdr_Size = 30;

Const
   WSIZE = 32768; (* window size--must be a power of two, and at least 32k *)
   PW_PROTECTED = 1;
   PKBACK = 'PKBACK# ';
   LHA_UNIXOS = $4D;
   ClipBoard_FillerBytes = 1; //256;

   HEAD_NONE = 0;
   HEAD_LOCAL = 1;
   HEAD_CENTRAL = 2;


// ztvZip
Const
   Z_VERSIONNUM = 20;
   Z_MINVERSIONNUM = 20;

// ztvArchiveSplitter & ztvZipSplitter
Const
	MIN_CUST_SIZE = 65536;
	ztv_OneKB = 1024;
	ztv_OneMB = ztv_OneKB * 1000;
   ztv_120_Floppy = 1213952;  //ztv_OneMB * 1.1855
   ztv_144_Floppy = 1457664;
   ztv_288_Floppy = 2915328;
   ztv_100_ZipDisk = ztv_OneMB * 100;
   ztv_250_ZipDisk = ztv_OneMB * 250;
   ztv_650_CDRom = ztv_OneMB * 650;
   ztv_700_CDRom = ztv_OneMB * 700;


Const
   // block types
   ACE_MAIN_BLK = 0;
   ACE_FILE_BLK = 1;
   ACE_REC_BLK = 2;

   // info in the Header Flag
   ACE_HDF_COMMENT = 1;                 // bit  1
   ACE_HDF_ANSI = 1024;                 // bit 11
   ACE_HDF_SPLIT_BEFORE = 2048;         // bit 12
   ACE_HDF_SPLIT_AFTER = 4096;          // bit 13
   ACE_HDF_PASSWORD = 8192;             // bit 14
   ACE_HDF_SOLID = 16384;               // bit 15

   ACECMT_OK = 0;
   ACECMT_SMALLBUF = 1;
   ACECMT_NONE = 255;

   // archive-header-flags
   ACE_LIM256 = 1024;
   ACE_MULT_VOL = 2048;
   ACE_AV = 4096;
   ACE_RECOV = 8192;
   ACE_LOCK = 16384;
   ACE_SOLID = 32768;

   // file-header-flags
   ACE_ADDSIZE = 1;
   ACE_COMMENT = 2;
   ACE_SP_BEFORE = 4096;
   ACE_SP_AFTER = 8192;
   ACE_PASSWORD = 16384;

   // known compression types
   ACE_TYPE_STORE = 0;
   ACE_TYPE_LZW1 = 1;

// RAR constants
Const
   (* RAR Archive header flags *)
   MHD_MULT_VOL = 1;
   MHD_COMMENT = 2;
   MHD_LOCK = 4;
   MHD_SOLID = 8;
   MHD_PACK_COMMENT = 16;
   MHD_NEWNUMBERING = 16;
   MHD_AV = 32;
   MHD_PROTECT = 64;

   LHD_LARGE = $0100;
   LHD_UNICODE = $0200;
   LHD_SALT = $0400;
   LHD_VERSION = $0800;
   LHD_EXTTIME = $1000;
   LHD_EXTFLAGS = $2000;

   (* RAR File header flags *)
   LHD_SPLIT_BEFORE = 1;
   LHD_SPLIT_AFTER = 2;
   LHD_PASSWORD = 4;
   LHD_COMMENT = 8;
   LHD_SOLID = 16;

   SKIP_IF_UNKNOWN = $4000;
   LONG_BLOCK = $8000;

   // rar header sizes
   SIZEOF_MARKHEAD = 7;
   SIZEOF_OLDMHD = 7;
   SIZEOF_NEWMHD = 13;
   SIZEOF_OLDLHD = 21;
   SIZEOF_NEWLHD = 32;
   SIZEOF_SHORTBLOCKHEAD = 7;
   SIZEOF_LONGBLOCKHEAD = 11;
   SIZEOF_COMMHEAD = 13;
   SIZEOF_PROTECTHEAD = 26;

   (* RAR internal block types *)
   ALL_HEAD = 0;
   MARK_HEAD = $72;                     // 114
   MAIN_HEAD = $73;                     // 115
   FILE_HEAD = $74;                     // 116
   COMM_HEAD = $75;                     // 117
   AV_HEAD = $76;                       // 118
   SUB_HEAD = $77;                      // 119
   PROTECT_HEAD = $78;                  // 120

Const
   // Tar linkflags - header types
   LF_OLDNORMAL = #0;                   // Normal disk file, Unix compat
   LF_NORMAL = '0';                     // Normal disk file
   LF_LINK = '1';                       // Link to previously dumped file
   LF_SYMLINK = '2';                    // Symbolic link
   LF_CHR = '3';                        // Character special file
   LF_BLK = '4';                        // Block special file
   LF_DIR = '5';                        // Directory
   LF_FIFO = '6';                       // FIFO special file
   LF_CONTIG = '7';                     // Contiguous file
   LF_VOL = 'V'; 								 // First record is volume label

Const
   ZTV_FILE_ATTRIBUTE_READONLY = $00000001;
   ZTV_FILE_ATTRIBUTE_HIDDEN = $00000002;
   ZTV_FILE_ATTRIBUTE_SYSTEM = $00000004;
   ZTV_FILE_ATTRIBUTE_DIRECTORY = $00000010;
   ZTV_FILE_ATTRIBUTE_ARCHIVE = $00000020;
   ZTV_FILE_ATTRIBUTE_DEVICE = $00000040;
   ZTV_FILE_ATTRIBUTE_NORMAL = $00000080;
   ZTV_FILE_ATTRIBUTE_TEMPORARY = $00000100;
   ZTV_FILE_ATTRIBUTE_SPARSE_FILE = $00000200;
   ZTV_FILE_ATTRIBUTE_REPARSE_POINT = $00000400;
   ZTV_FILE_ATTRIBUTE_COMPRESSED = $00000800;
   ZTV_FILE_ATTRIBUTE_OFFLINE = $00001000;
   ZTV_FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
   ZTV_FILE_ATTRIBUTE_ENCRYPTED = $00004000;

Const
   EndingHeader = $01;
   CentralHeader = $08;
   LocalHeader = $10;

Const
   MaxExtArray = 23;
   ExtArray: Array[0..MaxExtArray] Of WideString =
   ('.ACE', '.ARC', '.ARJ', '.BH', '.CAB', '.ENC', '.GZ', '.HA',
      '.JAR', '.LHA', '.LZH', '.PAK', '.PK3', '.PK_', '.RAR', '.TAR',
      '.TGZ', '.UUE', '.UU', '.WAR', '.XXE', '.Z', '.ZIP', '.ZOO');

Const
   Crc32Table: Array[0..255] Of u_long = (
      $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F,
      $E963A535, $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
      $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2,
      $F3B97148, $84BE41DE, $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
      $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC, $14015C4F, $63066CD9,
      $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
      $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
      $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
      $26D930AC, $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423,
      $CFBA9599, $B8BDA50F, $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
      $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D, $76DC4190, $01DB7106,
      $98D220BC, $EFD5102A, $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
      $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB, $086D3D2D,
      $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
      $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950,
      $8BBEB8EA, $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
      $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7,
      $A4D1C46D, $D3D6F4FB, $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
      $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9, $5005713C, $270241AA,
      $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409, $CE61E49F,
      $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
      $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
      $EAD54739, $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84,
      $0D6D6A3E, $7A6A5AA8, $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
      $F00F9344, $8708A3D2, $1E01F268, $6906C2FE, $F762575D, $806567CB,
      $196C3671, $6E6B06E7, $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
      $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8, $A1D1937E,
      $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
      $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55,
      $316E8EEF, $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
      $CC0C7795, $BB0B4703, $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28,
      $2BB45A92, $5CB36A04, $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
      $9B64C2B0, $EC63F226, $756AA39C, $026D930A, $9C0906A9, $EB0E363F,
      $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
      $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
      $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
      $88085AE6, $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69,
      $616BFFD3, $166CCF45, $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
      $A7672661, $D06016F7, $4969474D, $3E6E77DB, $AED16A4A, $D9D65ADC,
      $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
      $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605, $CDD70693,
      $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
      $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D
      );

Const
   CRC16Table: Array[0..255] Of word =
   (
      $0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241,
      $C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440,
      $CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40,
      $0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841,
      $D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40,
      $1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41,
      $1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641,
      $D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040,
      $F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240,
      $3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441,
      $3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41,
      $FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840,
      $2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41,
      $EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40,
      $E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640,
      $2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041,
      $A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240,
      $6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441,
      $6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41,
      $AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840,
      $7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41,
      $BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40,
      $B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640,
      $7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041,
      $5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241,
      $9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440,
      $9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40,
      $5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841,
      $8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40,
      $4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41,
      $4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641,
      $8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040
      );



Implementation





End.
