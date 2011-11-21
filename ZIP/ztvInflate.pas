Unit ztvInflate;

Interface

Uses
   Classes,
   SysUtils,
   ztvRegister,
   ztvBase,
   ztvStreams,
   ztvGbls,
   ztvConsts;

{$J+}    { Writeable Typed Constants }     // v4.0 added
{$Q-}    { Overflow Checking }  // v4.5.3 added

Function inflateInit_(z: ztv_streamp; DEF_WBITS: smallint; stream_size: _int): _int;
Function inflateEnd(Var z: ztv_stream): _int;
Function inflateReset(Var z: ztv_stream): _int;
Function _inflate(Var z: ztv_stream; f: _int): _int;
//Function inflateSetDictionary(Var z: ztv_stream; dictionary: _pBytef;
//	dictLength: uInt): _int;
Function ztvDecompress_String_Method1(s: String): String;
Function ztvDecompress_String_Method2(s: String; StrLength: Integer): String;
Function ztvDecompress_BufToBuf(Const inbuf: Pointer; InBytes: _int;
   Var outbuf: Pointer; Var outBytes: _int; OutEstimate: _int): u_long;
Function ztvDecompress_StreamToFile(Stream: TStream32; FileName: String): u_long;
Function ztvDecompress_StreamFromClipboard(Stream: TStream32): u_long;
Function ztvDecompress_StreamToStream(InStream, OutStream: TStream32;
	InBytes: _int): u_long;


Implementation

Uses
   ztvCrypt;

Const
   PRESET_DICT = $20;                   { preset dictionary flag in header }
   MANY = 1440;
   BMAX = {15} 16;                      { maximum bit length of any code }

Const
   inflate_mask: Array[0..16] Of _uInt = ($0000,
      $0001, $0003, $0007, $000F, $001F, $003F, $007F, $00FF,
      $01FF, $03FF, $07FF, $0FFF, $1FFF, $3FFF, $7FFF, $FFFF);

   invalid_code = {112; //} 99;  // v6.1 v4.8.5 revised
   border: Array[0..18] Of Word   { Order of the bit length code lengths }
   	= (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

	// Tables for deflate from PKZIP's appnote.txt.
	cplens64: Array[0..30] Of {u_long = //}_uInt = //v6.1
     (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
      35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 3, 0, 0);
   { the code 285 is defined differently }
   cplens32: Array[0..30] Of {u_long = //}_uInt = //v6.1    { Copy lengths for literal codes 257..285 }
     (3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
      35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);
   { actually lengths - 2; also see note #13 above about 258 }


	// Extra bits for literal codes 257..285
   cplext64: Array[0..30] Of {u_long = //}_uInt = //v6.1
     (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
   	3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 16, invalid_code, invalid_code);
   cplext32: Array[0..30] Of {u_long = //}_uInt = //v6.1
     (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
      3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0 , invalid_code, invalid_code);

   cpdist64: Array[0..31] Of {u_long = //}_uInt = //v6.1      { Copy offsets for distance codes 0..31 }
     (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
      257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
      8193, 12289, 16385, 24577, 32769, 49153);
   cpdist32: Array[0..29] Of {u_long = //}_uInt = //v6.1       { Copy offsets for distance codes 0..29 }
     (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
      257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
      8193, 12289, 16385, 24577);


   cpdext64: Array[0..31] Of {u_long = //}_uInt = //v6.1      { Extra bits for distance codes }
     (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
      7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13,
      14, 14);

   cpdext32: Array[0..29] Of {u_long = //}_uInt = //v6.1      { Extra bits for distance codes }
     (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
      7, 7, 8, 8, 9, 9, 10, 10, 11, 11,
      12, 12, 13, 13);


Const
   FIXEDH = 544;                        { number of hufts used by fixed tables }
   fixed_built: Boolean = False;        //v4.0 rem'd... changed to a global var

Var
   MaxDists: Byte;
   fixed_mem: Array[0..FIXEDH - 1] Of huft;
   fixed_bl: {u_long; //}_uInt; //v6.1
   fixed_bd: {u_long; //}_uInt; //v6.1
   fixed_tl: phuft;
   fixed_td: phuft;


//-------------------------------------------------------------

Function ZALLOC(Var strm: ztv_stream; Items: _uInt; size: _uInt): voidpf;
Begin
   ZALLOC := strm.ZALLOC(strm.opaque, Items, size);
End;
//-------------------------------------------------------------

Procedure ZFREE(Var strm: ztv_stream; ptr: voidpf);
Begin
   strm.ZFREE(strm.opaque, ptr);
End;
//-------------------------------------------------------------

Procedure inflate_codes_free(C: pInflate_codes_state; Var z: ztv_stream);
Begin
   ZFREE(z, C);
End;
//-------------------------------------------------------------

Procedure inflate_blocks_reset(Var s: inflate_blocks_state;
   Var z: ztv_stream;
   C: puLong);                          { check value on output }
Begin
   If (C <> Z_NULL) Then
      C^ := s.Check;

   // v6.5.1 rem'd "Or (s.mode = dtree)".  Using TZipKey with \3\zip\Encrypt_Xxxxx.zip,
   // password of "MhWEa" causes an unknown exception.  Attempts to release
   // memory that wasn't (at least in this review, couldn't find the allocation)
   // first allocated.
   If (s.mode = BTREE) {Or (s.mode = dtree)} Then
      ZFREE(z, s.sub.trees.blens);

   If (s.mode = CODES) Then
      inflate_codes_free(s.sub.decode.CODES, z);

   s.mode := ZTYPE;
   s.bitk := 0;
   s.bitb := 0;

   s.Write := s.window;
   s.Read := s.window;
   If Assigned(s.CheckFn) Then
   Begin
      s.Check := s.CheckFn(u_long(0), _pBytef(Nil), 0);
      z.adler := s.Check;
   End;
End;
//-------------------------------------------------------------

Function inflateReset(Var z: ztv_stream): _int;
Begin
   If (z.state = Z_NULL) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   z.total_out := 0;
   z.total_in := 0;

   If z.state^.nowrap Then
      z.state^.mode := BLOCKS
   Else
      z.state^.mode := method;

   inflate_blocks_reset(z.state^.BLOCKS^, z, Z_NULL);
   Result := Z_OK;
End;
//-------------------------------------------------------------

Function inflate_blocks_free(s: pInflate_blocks_state; Var z: ztv_stream): _int;
Begin
   inflate_blocks_reset(s^, z, Z_NULL);
   ZFREE(z, s^.window);
   ZFREE(z, s^.hufts);
   ZFREE(z, s);
   Result := Z_OK;
End;
//-------------------------------------------------------------

Function inflateEnd(Var z: ztv_stream): _int;
Begin
   If (z.state = Z_NULL) Or Not Assigned(z.ZFREE) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;
   If (z.state^.BLOCKS <> Z_NULL) Then
      inflate_blocks_free(z.state^.BLOCKS, z);

   ZFREE(z, z.state);
   z.state := Z_NULL;
   Result := Z_OK;
End;
//-------------------------------------------------------------

Function inflate_blocks_new(Var z: ztv_stream;
   C: _check_func;                      { check function }
   w: _uInt                              { window size }
   ): pInflate_blocks_state;
Var
   s: pInflate_blocks_state;
Begin
   s := pInflate_blocks_state(ZALLOC(z, 1, SizeOf(inflate_blocks_state)));
   If (s = Z_NULL) Then
   Begin
      Result := s;
      Exit;
   End;

   s^.hufts := huft_ptr(ZALLOC(z, SizeOf(huft), MANY));

   If (s^.hufts = Z_NULL) Then
   Begin
      ZFREE(z, s);
      Result := Z_NULL;
      Exit;
   End;

   s^.window := _pBytef(ZALLOC(z, 1, w));

   If (s^.window = Z_NULL) Then
   Begin
      ZFREE(z, s^.hufts);
      ZFREE(z, s);
      Result := Z_NULL;
      Exit;
   End;

   s^.zend := s^.window;
   inc(s^.zend, w);

   s^.CheckFn := C;
   s^.mode := ZTYPE;
   inflate_blocks_reset(s^, z, Z_NULL);
   Result := s;
End;
//-------------------------------------------------------------

Function inflateInit2_(Var z: ztv_stream;
   w: _int;
   stream_size: _int): _int;
Var
	BitMask: Byte;
Begin
	If is64Bit Then
   	MAXDISTS := 32
   Else

      MAXDISTS := 30;

   If (stream_size <> SizeOf(ztv_stream)) Then
   Begin
      Result := Z_VERSION_ERROR;
      Exit;
   End;

   { SetLength(strm.msg, 255); }
   If Not Assigned(z.ZALLOC) Then
   Begin
      z.ZALLOC := @ztvAllocMem;
      z.opaque := voidpf(0);
   End;

   If Not Assigned(z.ZFREE) Then
      z.ZFREE := @ztvFreeMem;

   z.state := pInternal_state(ZALLOC(z, 1, SizeOf(internal_state)));
   If (z.state = Z_NULL) Then
   Begin
      Result := Z_MEM_ERROR;
      Exit;
   End;

   z.state^.BLOCKS := Z_NULL;

   { handle undocumented nowrap option (no zlib header or check) }
   z.state^.nowrap := False;
   If (w < 0) Then
   Begin
      w := -w;
      z.state^.nowrap := True;
   End;

   BitMask := -MAX_WBITS;
   If is64Bit Then
   Begin
   	Inc(w);
      Inc(BitMask);
   End;
   
   { set window size }
   If (w < 8) Or (w > BitMask) Then
   Begin
      inflateEnd(z);
      Result := Z_STREAM_ERROR;
      Exit;
   End;
   z.state^.wbits := _uInt(w);

   { create inflate_blocks state }
   If z.state^.nowrap Then
      z.state^.BLOCKS := inflate_blocks_new(z, Nil, _uInt(1) Shl w)
   Else
      z.state^.BLOCKS := inflate_blocks_new(z, @_adler, _uInt(1) Shl w);

   If (z.state^.BLOCKS = Z_NULL) Then
   Begin
      inflateEnd(z);
      Result := Z_MEM_ERROR;
      Exit;
   End;

   inflateReset(z);
   Result := Z_OK;
End;
//-------------------------------------------------------------

{Function inflateInit2( Var z: ztv_stream; windowBits: SmallInt ): _int;
Begin
   Result := inflateInit2_( z, windowBits, SizeOf( ztv_stream ) );
End;}
//-------------------------------------------------------------

{Function inflateInit( Var z: ztv_stream; DEF_WBITS: SmallInt ): _int;
Begin
   Result := inflateInit2_( z, DEF_WBITS, SizeOf( ztv_stream ) );
End;}
//-------------------------------------------------------------

Function inflateInit_(z: ztv_streamp; DEF_WBITS: smallint; stream_size: _int): _int;
Begin
   If (z = Z_NULL) Then
      Result := Z_STREAM_ERROR
   Else
      Result := inflateInit2_(z^, DEF_WBITS, stream_size);
End;
//-------------------------------------------------------------

Function inflate_flush(Var s: inflate_blocks_state; Var z: ztv_stream;
   R: _int): _int;
Var
   n: _uInt;
   p, q: _pBytef;
Begin
   p := z.next_out;
   q := s.Read;

   { compute number of bytes to copy as far as end of window }
   If ptr2int(q) <= ptr2int(s.Write) Then
      n := _uInt(ptr2int(s.Write) - ptr2int(q))
   Else
      n := _uInt(ptr2int(s.zend) - ptr2int(q));

   If (n > z.avail_out) Then
      n := z.avail_out;

   If (n <> 0) And (R = Z_BUF_ERROR) Then
      R := Z_OK;

   Dec(z.avail_out, n);
   inc(z.total_out, n);

   If Assigned(s.CheckFn) Then
   Begin
      s.Check := s.CheckFn(s.Check, q, n);
      z.adler := s.Check;
   End;

   { copy as far as end of window }
   CopyMem(@q^, @p^, n);

   inc(p, n);
   inc(q, n);

   { see if more to copy at beginning of window }
   If (q = s.zend) Then
   Begin
      { wrap pointers }
      q := s.window;
      If (s.Write = s.zend) Then
         s.Write := s.window;

      { compute bytes to copy }
      n := _uInt(ptr2int(s.Write) - ptr2int(q));
      If (n > z.avail_out) Then
         n := z.avail_out;

      If (n <> 0) And (R = Z_BUF_ERROR) Then
         R := Z_OK;

      Dec(z.avail_out, n);
      inc(z.total_out, n);

      If Assigned(s.CheckFn) Then
      Begin
         s.Check := s.CheckFn(s.Check, q, n);
         z.adler := s.Check;
      End;

      CopyMem(@q^, @p^, n);
      inc(p, n);
      inc(q, n);
   End;

   z.next_out := p;
   s.Read := q;
   Result := R;
End;
//-------------------------------------------------------------

Function huft_build(
   Var b: Array Of {u_long; //}uIntf; //v6.1              { code lengths in bits (all assumed <= BMAX) }
   n: _uInt;                                          { number of codes (assumed <= N_MAX) }
   s: _uInt;                                          { number of simple-valued codes (0..s-1) }
   Const d: Array Of {u_long; //}uIntf;  //v6.1           { list of base values for non-simple codes }
   Const e: Array Of {u_long; //}uIntf;  //v6.1           { list of extra bits for non-simple codes (array of word )}
   t: pphuft;                                         { result: starting table (array of byte) }
   Var m: {u_long; //}uIntf; //v6.1                       { maximum lookup bits, returns actual }
   Var hp: Array Of huft;                             { space for trees }
   Var hn: _uInt;                                     { hufts used in space }
   Var v: Array Of {u_long //}uIntf                     { working area: values in order of bit length }
   ): _int;
Const
//{$ifndef LocalDef64}
	AdditionalBitsInTable = 0;
	InvalidCode = 128 + 64;
   EobCode = 32 + 64;
   ValidCode = 0;
   NonSimpleLookup = 16 + 64;
//{$else}
//	AdditionalBitsInTable = 32;
//	InvalidCode = 99;
//   EobCode = 31;
//   ValidCode = 32;
//   NonSimpleLookup = 0;
//{$endif}
Var
   a: _uInt;                             	{ counter for codes of length k }
   C: Array[0..BMAX] Of _uInt;           	{ bit length count table }
   f: _uInt;                             	{ i repeats in table every f entries }
   g: _int;                             	{ maximum code length }
   H: _int;                             	{ table level }
   i: _uInt; {register}                 	{ counter, current code }
   j: _uInt; {register}                 	{ counter }
   k: _int; {register}                  	{ number of bits in current code }
   l: uintf; //_int;                    	{ bits per table (returned in m) }
   Mask: _uInt;                         	{ (1 shl w) - 1, to avoid cc -O bug on HP }
   p: ^uIntf; {register}                	{ pointer into c[], b[], or v[] }
   q: phuft;                            	{ points to current table }
   R: huft;                            	{ table entry for structure assignment }
   u: Array[0..BMAX - 1] Of phuft;        { table stack }
   w: _int; {register}                  	{ bits before this table = (l*h) }
   x: Array[0..BMAX] Of _uInt;           	{ bit offsets, then code stack }
   xp: puIntf;                          	{ pointer into x }
   y: _int;                             	{ number of dummy codes added }
   z: _uInt;                             	{ number of entries in current table }
Begin
   FillChar(C, SizeOf(C), 0);           	{ clear c[] }

   For i := 0 To n - 1 Do
      inc(C[b[i]]);                     	{ assume all entries <= BMAX }

   If (C[0] = n) Then                   	{ null input--all zero length codes }
   Begin
      t^ := phuft(Nil);
      m := 0;
      Result := Z_OK;
      Exit;
   End;

   { Find minimum and maximum length, bound [m] by those }
   l := m;
   For j := 1 To BMAX Do
      If (C[j] <> 0) Then
         break;

   k := j;                              	{ minimum code length }
   If (_uInt(l) < j) Then
     l := j;

   For i := BMAX Downto 1 Do
      If (C[i] <> 0) Then
         break;

   g := i;                              	{ maximum code length }
   If (_uInt(l) > i) Then
      l := i;

   m := l;

   { Adjust last length count to fill out codes, if needed }
   y := 1 Shl j;
   While (j < i) Do
   Begin
      Dec(y, C[j]);
      If (y < 0) Then
      Begin
         Result := Z_DATA_ERROR;        	{ bad input: more codes than bits }
         Exit;
      End;
      inc(j);
      y := y Shl 1
   End;

   Dec(y, C[i]);
   If (y < 0) Then
   Begin
      Result := Z_DATA_ERROR;           	{ bad input: more codes than bits }
      Exit;
   End;
   inc(C[i], y);


   { Generate starting offsets into the value table for each length }
   x[1] := 0;
   j := 0;

   p := @C[1];
   xp := @x[2];

   Dec(i);                              	{ note that i = g from above }
   While (i > 0) Do
   Begin
      inc(j, p^);
      xp^ := j;
      inc(p);
      inc(xp);
      Dec(i);
   End;

   { Make a table of values in order of bit lengths }
   For i := 0 To n - 1 Do
   Begin
      j := b[i];
      If (j <> 0) Then
      Begin
         v[x[j]] := i;
         inc(x[j]);
      End;
   End;
   n := x[g];                           	{ set n to length of v }

   { Generate the Huffman codes and for each, make the table entries }
   i := 0;
   x[0] := 0;                           	{ first Huffman code is zero }
   p := Addr(v);                        	{ grab values in bit order }
   H := -1;                             	{ no tables yet--level -1 }
   w := -l;                             	{ bits decoded = (l*h) }

   u[0] := phuft(Nil);          				{ just to keep compilers happy }
   q := phuft(Nil);                    	{ ditto }
   z := 0;                              	{ ditto }

   { go through the bit lengths (k already is bits in shortest code) }
   While (k <= g) Do
   Begin
      a := C[k];
      While (a <> 0) Do
      Begin
         Dec(a);

         { here i is the Huffman code of length k bits for value p^ }
         { make tables up to required level }
         While (k > w + l) Do
         Begin

            inc(H);
            inc(w, l);                  	{ add bits already decoded }
            										{ previous table always l bits }
            										{ compute minimum size table less than or equal to l bits }

            { table size upper limit }
            z := g - w;

            If (z > _uInt(l)) Then
               z := l;

            { try a k-w bit table }
            j := k - w;
            f := 1 Shl j;
            If (f > a + 1) Then         	{ too few codes for k-w bit table }
            Begin
               Dec(f, a + 1);           	{ deduct codes from patterns left }

               xp := Addr(C[k]);
               If (j < z) Then
               Begin
                  inc(j);
                  While (j < z) Do
                  Begin                 	{ try smaller tables up to z bits }
                     f := f Shl 1;
                     inc(xp);
                     If (f <= xp^) Then
                        break;          	{ enough codes to use up j bits }
                     Dec(f, xp^);       	{ else deduct codes from patterns }
                     inc(j);
                  End;
               End;
            End;

            z := 1 Shl j;               	{ table entries for j-bit table }

            { allocate new table }
            If (hn + z > MANY) Then     	{ (note: doesn't matter for fixed) }
            Begin
               Result := Z_MEM_ERROR;   	{ not enough memory }
               Exit;
            End;

            q := @hp[hn];
            u[H] := q;
            inc(hn, z);

            { connect to last table, if there is one }
            If (H <> 0) Then
            Begin
               x[H] := i;               	{ save pattern for backing up }
               R.b := Byte(l);          	{ bits to dump before this table }
               R.e := Byte(AdditionalBitsInTable+j);       { bits in this table }
					{.$ifdef LocalDef64}
                              				// ==========
					//R.Pad := 101;				// next table
                              				// ==========
					{.$endif}
               j := i Shr (w - l);
               {r.v_n := _uInt( q - u[h-1] -j);}{ offset to this table }
               R.v_n := (ptr2int(q) - ptr2int(u[H - 1])) Div SizeOf(q^) - j;
               huft_ptr(u[H - 1])^[j] := R; { connect to last table }
            End
            Else
               t^ := q;                 	{ first table is returned result }
         End;

         { set up table entry in r }
         R.b := Byte(k - w);

         If ptr2int(p) >= ptr2int(@(v[n])) Then { also works under DPMI ?? }
         Begin
            R.e := InvalidCode;      		{ out of values--invalid code }
				{.$ifdef LocalDef64}       	// ============
            //R.pad := 99;           		// Invalid Code
				{.$endif}                  	// ============
         End Else
            If (p^ < s) Then
            Begin
               If (p^ < 256) Then       	{ 256 is end-of-block code }
               Begin
                  R.e := ValidCode;
						{.$ifdef LocalDef64} 	// =======
						//R.pad := 102;			// literal
						{.$endif}            	// =======
               End Else Begin
                  R.e := EobCode;
						{.$ifdef LocalDef64} 	// =======================
						//R.pad := 103;			// EOB - end of block code
						{.$endif}            	// =======================
					End;

               R.v_n := p^;            	{ simple code is just the value }
               inc(p);
            End Else Begin
               R.e := Byte(e[p^ - s] + NonSimpleLookup); { non-simple--look up in lists }
               R.v_n := d[p^ - s];
               {.$ifdef LocalDef64}    	// ======
               //R.pad := 105;				// length
               {.$endif}               	// ======
               inc(p);
            End;

         { fill code-like entries with r }
         f := 1 Shl (k - w);
         j := i Shr w;
         While (j < z) Do
         Begin
            huft_ptr(q)^[j] := R;
            inc(j, f);
         End;

         { backwards increment the k-bit code i }
         j := 1 Shl (k - 1);
         While (i And j) <> 0 Do
         Begin
            i := i Xor j;               	{ bitwise exclusive or }
            j := j Shr 1
         End;
         i := i Xor j;

         { backup over finished tables }
         Mask := (1 Shl w) - 1;         	{ needed on HP, cc -O bug }
         While ((i And Mask) <> x[H]) Do
         Begin
            Dec(H);                     	{ don't need to update q }
            Dec(w, l);
            Mask := (1 Shl w) - 1;
         End;

      End;
      inc(k);
   End;

   If (y <> 0) And (g <> 1) Then
      Result := Z_BUF_ERROR
   Else
      Result := Z_OK;
End;
//-------------------------------------------------------------

Function inflate_trees_fixed(
   Var bl: {u_long; //}_uInt; //v6.1        { literal desired/actual bit depth }
   Var bd: {u_long; //}_uInt; //v6.1        { distance desired/actual bit depth }
   Var tl: phuft;               			 { literal/length tree result }
   Var td: phuft;               			 { distance tree result }
   Var z: ztv_stream                    { for memory allocation }
   ): _int;
Type
   pFixed_table = ^fixed_table;
   fixed_table = Array[0..288 - 1] Of {u_long; //}uIntf; //v6.1
Var
   k: _int;                             { temporary variable }
   C: pFixed_table;                     { length list for huft_build }
   v: PuIntArray;                       { work area for huft_build }
   f: _uInt;                            { number of hufts used in fixed_mem }
Begin
   { build fixed tables if not already (multiple overlapped executions ok) }
   If Not fixed_built Then
   Begin
      f := 0;

      { allocate memory }
      C := pFixed_table(ZALLOC(z, 288, SizeOf(_uInt)));
      If (C = Z_NULL) Then
      Begin
         Result := Z_MEM_ERROR;
         Exit;
      End;

      v := PuIntArray(ZALLOC(z, 288, SizeOf(_uInt)));
      If (v = Z_NULL) Then
      Begin
         ZFREE(z, C);
         Result := Z_MEM_ERROR;
         Exit;
      End;

      { literal table }
      For k := 0 To Pred(144) Do
         C^[k] := 8;
      For k := 144 To Pred(256) Do
         C^[k] := 9;
      For k := 256 To Pred(280) Do
         C^[k] := 7;
      For k := 280 To Pred(288) Do
         C^[k] := 8;

      fixed_bl := {9; //temp} 7; //9;  v4.8.5 changed

      If is64Bit Then
         huft_build(C^, 288, 257, cplens64, cplext64, @fixed_tl, fixed_bl,
            fixed_mem, f, v^)
      Else
         huft_build(C^, 288, 257, cplens32, cplext32, @fixed_tl, fixed_bl,
            fixed_mem, f, v^);

      { distance table }
      For k := 0 To MAXDISTS - 1 Do
         C^[k] := 5;

      fixed_bd := 5;

      If is64Bit Then
         huft_build(C^, MAXDISTS, 0, cpdist64, cpdext64, @fixed_td, fixed_bd,
            fixed_mem, f, v^)
      Else
         huft_build(C^, MAXDISTS, 0, cpdist32, cpdext32, @fixed_td, fixed_bd,
            fixed_mem, f, v^);


      ZFREE(z, v);
      ZFREE(z, C);

      // **************************************************
      // if your compiler stops here during compile:
      // 	goto menu Project/Options/Compiler and check
      //    the "Assignable typed constants" checkbox.
      fixed_built := True;
      // **************************************************

   End;
   bl := fixed_bl;
   bd := fixed_bd;
   tl := fixed_tl;
   td := fixed_td;
   Result := Z_OK;
End;
//-------------------------------------------------------------

Function inflate_codes_new(bl, bd: _uInt; tl, td: phuft; Var z: ztv_stream):
	pInflate_codes_state;
Var
   C: pInflate_codes_state;
Begin
   C := pInflate_codes_state(ZALLOC(z, 1, SizeOf(inflate_codes_state)));
   If (C <> Z_NULL) Then
   Begin
      C^.mode := START;
      C^.lbits := Byte(bl);
      C^.dbits := Byte(bd);
      C^.ltree := tl;
      C^.dtree := td;
   End;
   Result := C;
End;
//-------------------------------------------------------------

Function inflate_trees_bits(
   Var C: Array Of {u_long; //}uIntf; //v6.1  { 19 code lengths }
   Var bb: {u_long; //}uIntf;  //v6.1         { bits tree desired/actual depth }
   Var tb: phuft;               			 	{ bits tree result }
   Var hp: Array Of huft;       				{ space for trees }
   Var z: ztv_stream                    	{ for messages }
   ): _int;
Var
   R: _int;
   hn: _uInt;                            	{ hufts used in space }
   v: PuIntArray;                       	{ work area for huft_build }
Begin
   hn := 0;
   v := PuIntArray(ZALLOC(z, 19, SizeOf(_uInt)));
   If (v = Z_NULL) Then
   Begin
      Result := Z_MEM_ERROR;
      Exit;
   End;

   Try

      If is64Bit Then
         R := huft_build(C, 19, 19, cplens64, cplext64,
            @tb, bb, hp, hn, v^)
      Else
         R := huft_build(C, 19, 19, cplens32, cplext32,
            @tb, bb, hp, hn, v^);

      If (R = Z_DATA_ERROR) Then
         {oversubscribed dynamic bit lengths tree}
      Else
         If (R = Z_BUF_ERROR) Or (bb = 0) Then
         Begin
         	{incomplete dynamic bit lengths tree}
            R := Z_DATA_ERROR;
         End;
   Finally
   	ZFREE(z, v);
   End;
   Result := R;
End;
//-------------------------------------------------------------

Function inflate_trees_dynamic(
   nl: {u_long; //}_uInt; //v6.1           { number of literal/length codes }
   nd: {u_long; //}_uInt; //v6.1           { number of distance codes }
   Var C: Array Of {u_long; //}uIntf; //v6.1 { that many (total) code lengths }
   Var bl: {u_long; //}uIntf; //v6.1       { literal desired/actual bit depth }
   Var bd: {u_long; //}uIntf; //v6.1       { distance desired/actual bit depth }
   Var tl: phuft;               			{ literal/length tree result }
   Var td: phuft;               			{ distance tree result }
   Var hp: Array Of huft;      			{ space for trees }
   Var z: ztv_stream                   { for messages }
   ): _int;
Var
   R: _int;
   hn: _uInt;                          { hufts used in space }
   v: PuIntArray;                      { work area for huft_build }
Begin
   hn := 0;

   { allocate work area }
   v := PuIntArray(ZALLOC(z, 288, SizeOf(_uInt)));
   If (v = Z_NULL) Then
   Begin
      Result := Z_MEM_ERROR;
      Exit;
   End;

   { build literal/length tree }
   If is64Bit Then
   	R := huft_build(C, nl, 257, cplens64, cplext64, @tl, bl, hp, hn, v^)
   Else
   	R := huft_build(C, nl, 257, cplens32, cplext32, @tl, bl, hp, hn, v^);

   If (R <> Z_OK) Or (bl = 0) Then
   Begin
      If (R = Z_DATA_ERROR) Then
         {oversubscribed literal/length tree}
      Else
         If (R <> Z_MEM_ERROR) Then
         Begin
            {incomplete literal/length tree}
            R := Z_DATA_ERROR;
         End;

      ZFREE(z, v);
      Result := R;
      Exit;
   End;

   { build distance tree }
   If is64Bit Then
      R := huft_build(PuIntArray(@C[nl])^, nd, 0,
         cpdist64, cpdext64, @td, bd, hp, hn, v^)
   Else
      R := huft_build(PuIntArray(@C[nl])^, nd, 0,
         cpdist32, cpdext32, @td, bd, hp, hn, v^);

   If (R <> Z_OK) Or ((bd = 0) And (nl > 257)) Then
   Begin
      If (R = Z_DATA_ERROR) Then
         {oversubscribed literal/length tree}
      Else
         If (R = Z_BUF_ERROR) Then
         Begin
            {incomplete literal/length tree}
            R := Z_DATA_ERROR;
         End
         Else
         Begin
            If (R <> Z_MEM_ERROR) Then
               {empty distance tree with lengths}
               R := Z_DATA_ERROR;
         End;

      ZFREE(z, v);
      Result := R;
      Exit;
   End;

   ZFREE(z, v);
   Result := Z_OK;
End;
//-------------------------------------------------------------

Function inflate_fast(bl: _uInt; bd: _uInt; tl: phuft; td: phuft;
	Var s: inflate_blocks_state; Var z: ztv_stream): _int;
Var
   t: phuft;                    				{ temporary pointer }
   e: _uInt;                             	{ extra bits or operation }
   b: u_long;                           	{ bit buffer }
   k: _uInt;                             	{ bits in bit buffer }
   p: _pBytef;                          	{ input data pointer }
   n: _uInt;                             	{ bytes available there }
   q: _pBytef;                          	{ output window write pointer }
   m: _uInt;                             	{ bytes to end of window or read pointer }
   ml: _uInt;                            	{ mask for literal/length tree }
   md: _uInt;                           	{ mask for distance tree }
   // C must be an integer... see notes for TZipKey in this function.
   C: Integer; //_uInt;                  	{ bytes to copy }
   d: _uInt;                             	{ distance back to copy from }
   R: _pBytef;                          	{ copy source pointer }

   Procedure NeedBits(j: _uInt);
   Begin
      While (k < j) Do
      Begin
         Dec(n);
         b := b Or (u_long(p^) Shl k);
         inc(p);
         inc(k, 8);
      End;
   End;

   Procedure DUMPBITS(n: Byte);
   Begin
      b := b Shr n;
      k := k - n;
   End;

   Procedure Update;
   Begin
   	//UPDBITS();
   	s.bitb := b;
      s.bitk := k;

      //UPDIN();
      z.avail_in := n;
      inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
      z.next_in := p;

      //UPDOUT();
   	s.write := q;
   End;

   //define UNGRAB //n+=(c=k>>3);p-=c;k&=7;
   Procedure UNGRAB;
   Begin
   	C := z.avail_in - n;
   	If (k Shr 3) < C Then
      	C := k Shr 3;
   	//C := k Shr 3;

      If C > 0 Then
      Begin
      	Inc(n, C);
      	Dec(p, C);
      	Dec(k, C Shl 3);
      	//k := k And 7;
      End;
   End;
Begin

   { load input, output, bit values (macro LOAD) }
   p := z.next_in;
   n := z.avail_in;
   b := s.bitb;
   k := s.bitk;
   q := s.Write;

   If ptr2int(q) < ptr2int(s.Read) Then
      m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
   Else
      m := _uInt(ptr2int(s.zend) - ptr2int(q));

   { initialize masks }
   ml := inflate_mask[bl];
   md := inflate_mask[bd];

   { do until not enough input or output space for fast loop }
   Repeat                               { assume called with (m >= 258) and (n >= 10) }
      { get literal/length code }
{.$ifndef LocalDef64}
    	NeedBits(20);                // max bits for literal/length code
{.$else}
//    	NeedBits(bl);                // max bits for literal/length code
{.$endif}
      t := @(huft_ptr(tl)^[_uInt(b) And ml]);
      e := t^.e;

      If (e = 0) Then
      Begin
         DumpBits(t^.b);
         q^ := Byte(t^.v_n);
         inc(q);
         Dec(m);
         Continue;
      End;

      Repeat
      	DUMPBITS(t^.b);

{.$ifndef LocalDef64}
         If (e And 16 <> 0) Then
{.$else}
//         If (e And 32 <> 0) Then
{.$endif}
         Begin
            { get extra bits for length }
{.$ifndef LocalDef64}
            e := e And 15;
{.else}
//            e := e And 31;
{.$endif}
            C := t^.v_n + (_uInt(b) And inflate_mask[e]);

            DumpBits(e);
            { decode distance base of block to copy }
            NeedBits(15);

            t := @huft_ptr(td)^[_uInt(b) And md];
            e := t^.e;

            Repeat
            	DumpBits(t^.b);

               If (e And 16 <> 0) Then
               Begin

                  { get extra bits to add to distance base }
                  e := e And 15;
            		NeedBits(e);         // get extra bits (up to 13)

                  d := t^.v_n + (_uInt(b) And inflate_mask[e]);
            		DUMPBITS(e);

                  { do the copy }
                  Dec(m, C);
                  If (_uInt(ptr2int(q) - ptr2int(s.window)) >= d) Then { offset before dest }
                  Begin                 {  just copy }
                     R := q;
                     Dec(R, d);
                     q^ := R^; inc(q); inc(R); Dec(C); // minimum count is three,
                     q^ := R^; inc(q); inc(R); Dec(C); // so unroll loop a little
                  End Else Begin			 // else offset after destination
                     e := d - _uInt(ptr2int(q) - ptr2int(s.window)); { bytes from offset to end }
                     R := s.zend;
                     Dec(R, e);         { pointer to offset }
                     If (C > e) Then    { if source crosses, }
                     Begin
                        Dec(C, e);      { copy to end of window }
                        Repeat
                           q^ := R^;
                           inc(q);
                           inc(R);
                           Dec(e);
                        Until (e = 0);
                        R := s.window;  { copy rest from start of window }
                     End;
                  End;

                  Repeat                { copy all or what's left }
                     q^ := R^;
                     inc(q);
                     inc(R);
                     Dec(C);
                  //Until (C = 0);
                  Until (C <= 0);		// v6.4.3 - TZipKey brute force method causes c to be less than 0 at times.

                  break;
               End Else
                  If (e And 64 = 0) Then
                  Begin
                     inc(t, t^.v_n + (_uInt(b) And inflate_mask[e]));
                     e := t^.e;
                  End Else Begin
                  	//invalid distance code
                     UNGRAB();
                     UPDATE();
                     Result := Z_DATA_ERROR;
                     Exit;
                  End;
            Until False;
            break;
         End;

         If (e And 64 = 0) Then
         Begin
            inc(t, t^.v_n + (_uInt(b) And inflate_mask[e]));
            e := t^.e;
            If (e = 0) Then
            Begin
          		DUMPBITS(t^.b);
               q^ := Byte(t^.v_n);
               inc(q);
               Dec(m);
               break;
            End;
         End Else
            If (e And 32 <> 0) Then
            Begin
               UNGRAB();
               UPDATE();
               Result := Z_STREAM_END;
               Exit;
            End Else Begin
            	//invalid literal/length code
               UNGRAB();
               UPDATE();
               Result := Z_DATA_ERROR;
               Exit;
            End;
      Until False;
   Until (m < 258) Or (n < 10);

   { not enough input or output--restore pointers and return }
   UNGRAB();
   UPDATE();
   Result := Z_OK;
End;
//-------------------------------------------------------------

Function inflate_codes(Var s: inflate_blocks_state;
   Var z: ztv_stream;
   R: _int): _int;
Var
   j: _uInt;                             { temporary storage }
   t: phuft;                     		  { temporary pointer }
   e: _uInt;                             { extra bits or operation }
   b: u_long;                            { bit buffer }
   k: _uInt;                             { bits in bit buffer }
   p: _pBytef;                           { input data pointer }
   n: _uInt;                             { bytes available there }
   q: _pBytef;                           { output window write pointer }
   m: _uInt;                             { bytes to end of window or read pointer }
   f: _pBytef;                           { pointer to copy strings from }

   C: pInflate_codes_state;
Begin
   C := s.sub.decode.CODES;

   p := z.next_in;
   n := z.avail_in;
   b := s.bitb;
   k := s.bitk;
   q := s.Write;

   If ptr2int(q) < ptr2int(s.Read) Then
      m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
   Else
      m := _uInt(ptr2int(s.zend) - ptr2int(q));

   While True Do
      Case (C^.mode) Of
         START:
            Begin
               If (m >= 258) And (n >= 10) Then
               Begin
                  s.bitb := b;
                  s.bitk := k;
                  z.avail_in := n;
                  inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                  z.next_in := p;
                  s.Write := q;

                  R := inflate_fast(C^.lbits, C^.dbits, C^.ltree, C^.dtree, s, z);
                  {LOAD}
                  p := z.next_in;
                  n := z.avail_in;
                  b := s.bitb;
                  k := s.bitk;
                  q := s.Write;
                  If ptr2int(q) < ptr2int(s.Read) Then
                     m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                  Else
                     m := _uInt(ptr2int(s.zend) - ptr2int(q));

                  If (R <> Z_OK) Then
                  Begin
                     If (R = Z_STREAM_END) Then
                        C^.mode := WASH
                     Else
                        C^.mode := BADCODE;
                     Continue;
                  End;
               End;
               C^.sub.code.need := C^.lbits;
               C^.sub.code.tree := C^.ltree;
               C^.mode := Len;
            End;
         Len:                           { i: get length/literal/eob next }
            Begin
               j := C^.sub.code.need;

               //NeedBits(j); + read
               While (k < j) Do
               Begin
                  If (n <> 0) Then
                     R := Z_OK
                  Else
                  Begin
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;
                  Dec(n);
                  b := b Or (u_long(p^) Shl k);
                  inc(p);
                  inc(k, 8);
               End;

               t := C^.sub.code.tree;
               inc(t, _uInt(b) And inflate_mask[j]);
               b := b Shr t^.b;
               Dec(k, t^.b);

               e := _uInt(t^.e);
{.$ifndef LocalDef64}
               If (e = 0) Then
{.$else}
//               If (e = 32) Then
{.$endif}
               Begin
                                    // =======
               	// t^.pad = 102	// literal
                  						// =======
                  C^.sub.lit := t^.v_n;
                  C^.mode := lit;
                  Continue;
               End;

					{.$ifndef LocalDef64}
               If (e And 16 <> 0) Then
					{.$else}
	            //If (e And 32 <> 0) Then
					{.$endif}
               Begin
               							// ======
               	// t^.pad = 105	// length
                  						// ======
						{.$ifndef LocalDef64}
                  C^.sub.Copy.get := e And 15;
                  {.$else}
                  //C^.sub.Copy.get := e And 31;
                  {.$endif}
                  C^.Len := t^.v_n;
                  C^.mode := LENEXT;
                  Continue;
               End;

               If (e And 64 = 0) Then
               Begin
               							// ==========
               	// t^.pad = 101	// next table
               							// ==========
                  C^.sub.code.need := e;
                  C^.sub.code.tree := @huft_ptr(t)^[t^.v_n];
                  Continue;
               End;

               If (e And 32 <> 0) Then
               Begin
               							// ==================
               	// t^.pad := 103  // EOB - end of block
               							// ==================
                  C^.mode := WASH;
                  Continue;
               End;

               // ===========================
               // invalid literal/length code
               // ===========================
               C^.mode := BADCODE;
               R := Z_DATA_ERROR;
               s.bitb := b;
               s.bitk := k;
               z.avail_in := n;
               inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
               z.next_in := p;
               s.Write := q;
               Result := inflate_flush(s, z, R);
               Exit;
            End;
         LENEXT:                        { i: getting length extra (have base) }
            Begin
               j := C^.sub.Copy.get;
               While (k < j) Do
               Begin
                  If (n <> 0) Then
                     R := Z_OK
                  Else
                  Begin
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;
                  Dec(n);
                  b := b Or (u_long(p^) Shl k);
                  inc(p);
                  inc(k, 8);
               End;
               inc(C^.Len, _uInt(b And inflate_mask[j]));
               b := b Shr j;
               Dec(k, j);

               C^.sub.code.need := C^.dbits;
               C^.sub.code.tree := C^.dtree;
               C^.mode := dist;
            End;
         dist:                          { i: get distance next }
            Begin
               j := C^.sub.code.need;
               While (k < j) Do
               Begin
                  If (n <> 0) Then
                     R := Z_OK
                  Else
                  Begin
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;
                  Dec(n);
                  b := b Or (u_long(p^) Shl k);
                  inc(p);
                  inc(k, 8);
               End;
               t := @huft_ptr(C^.sub.code.tree)^[_uInt(b) And inflate_mask[j]];
               b := b Shr t^.b;
               Dec(k, t^.b);

               e := _uInt(t^.e);
               If (e And 16 <> 0) Then  { distance }
               Begin
                  C^.sub.Copy.get := e And 15;
                  C^.sub.Copy.dist := t^.v_n;
                  C^.mode := DISTEXT;
                  Continue;             { break C-switch statement }
               End;
               If (e And 64 = 0) Then   { next table }
               Begin
                  C^.sub.code.need := e;
                  C^.sub.code.tree := @huft_ptr(t)^[t^.v_n];
                  Continue;             { break C-switch statement }
               End;
               C^.mode := BADCODE;      { invalid code }
               //invalid distance code
               R := Z_DATA_ERROR;
               s.bitb := b;
               s.bitk := k;
               z.avail_in := n;
               inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
               z.next_in := p;
               s.Write := q;
               Result := inflate_flush(s, z, R);
               Exit;
            End;
         DISTEXT:                       { i: getting distance extra }
            Begin
               j := C^.sub.Copy.get;
               While (k < j) Do
               Begin
                  If (n <> 0) Then
                     R := Z_OK
                  Else
                  Begin
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;
                  Dec(n);
                  b := b Or (u_long(p^) Shl k);
                  inc(p);
                  inc(k, 8);
               End;
               inc(C^.sub.Copy.dist, _uInt(b) And inflate_mask[j]);
               b := b Shr j;
               Dec(k, j);
               C^.mode := zCOPY;
            End;
         zCOPY:                         { o: copying bytes in window, waiting for space }
            Begin
               f := q;
               Dec(f, C^.sub.Copy.dist);
               If (_uInt(ptr2int(q) - ptr2int(s.window)) < C^.sub.Copy.dist) Then
               Begin
                  f := s.zend;
                  Dec(f, C^.sub.Copy.dist - _uInt(ptr2int(q) - ptr2int(s.window)));
               End;

               While (C^.Len <> 0) Do
               Begin
                  If (m = 0) Then
                  Begin
                     If (q = s.zend) And (s.Read <> s.window) Then
                     Begin
                        q := s.window;
                        If ptr2int(q) < ptr2int(s.Read) Then
                           m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                        Else
                           m := _uInt(ptr2int(s.zend) - ptr2int(q));
                     End;

                     If (m = 0) Then
                     Begin
                        {FLUSH}
                        s.Write := q;
                        R := inflate_flush(s, z, R);
                        q := s.Write;
                        If ptr2int(q) < ptr2int(s.Read) Then
                           m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                        Else
                           m := _uInt(ptr2int(s.zend) - ptr2int(q));

                        If (q = s.zend) And (s.Read <> s.window) Then
                        Begin
                           q := s.window;
                           If ptr2int(q) < ptr2int(s.Read) Then
                              m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                           Else
                              m := _uInt(ptr2int(s.zend) - ptr2int(q));
                        End;

                        If (m = 0) Then
                        Begin
                           s.bitb := b;
                           s.bitk := k;
                           z.avail_in := n;
                           inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                           z.next_in := p;
                           s.Write := q;
                           Result := inflate_flush(s, z, R);
                           Exit;
                        End;
                     End;
                  End;
                  R := Z_OK;

                  q^ := f^;
                  inc(q);
                  inc(f);
                  Dec(m);

                  If (f = s.zend) Then
                     f := s.window;
                  Dec(C^.Len);
               End;
               C^.mode := START;
            End;
         lit:                           { o: got literal, waiting for output space }
            Begin
               If (m = 0) Then
               Begin
                  If (q = s.zend) And (s.Read <> s.window) Then
                  Begin
                     q := s.window;
                     If ptr2int(q) < ptr2int(s.Read) Then
                        m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                     Else
                        m := _uInt(ptr2int(s.zend) - ptr2int(q));
                  End;

                  If (m = 0) Then
                  Begin
                     {FLUSH}
                     s.Write := q;
                     R := inflate_flush(s, z, R);
                     q := s.Write;
                     If ptr2int(q) < ptr2int(s.Read) Then
                        m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                     Else
                        m := _uInt(ptr2int(s.zend) - ptr2int(q));

                     If (q = s.zend) And (s.Read <> s.window) Then
                     Begin
                        q := s.window;
                        If ptr2int(q) < ptr2int(s.Read) Then
                           m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                        Else
                           m := _uInt(ptr2int(s.zend) - ptr2int(q));
                     End;

                     If (m = 0) Then
                     Begin
                        s.bitb := b;
                        s.bitk := k;
                        z.avail_in := n;
                        inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                        z.next_in := p;
                        s.Write := q;
                        Result := inflate_flush(s, z, R);
                        Exit;
                     End;
                  End;
               End;
               R := Z_OK;

               q^ := C^.sub.lit;
               inc(q);
               Dec(m);

               C^.mode := START;
            End;
         WASH:                          { o: got eob, possibly more output }
            Begin

               {FLUSH}
               s.Write := q;
               R := inflate_flush(s, z, R);
               q := s.Write;
               If ptr2int(q) < ptr2int(s.Read) Then
                  m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
               Else
                  m := _uInt(ptr2int(s.zend) - ptr2int(q));

               If (s.Read <> s.Write) Then
               Begin
                  s.bitb := b;
                  s.bitk := k;
                  z.avail_in := n;
                  inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                  z.next_in := p;
                  s.Write := q;
                  Result := inflate_flush(s, z, R);
                  Exit;
               End;
               C^.mode := zend;
            End;

         zend:
            Begin
               R := Z_STREAM_END;
               s.bitb := b;
               s.bitk := k;
               z.avail_in := n;
               inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
               z.next_in := p;
               s.Write := q;
               Result := inflate_flush(s, z, R);
               Exit;
            End;
         BADCODE:                       { x: got error }
            Begin
               R := Z_DATA_ERROR;
               s.bitb := b;
               s.bitk := k;
               z.avail_in := n;
               inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
               z.next_in := p;
               s.Write := q;
               Result := inflate_flush(s, z, R);
               Exit;
            End;
      Else
         Begin
            R := Z_STREAM_ERROR;
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
            z.next_in := p;
            s.Write := q;
            Result := inflate_flush(s, z, R);
            Exit;
         End;
      End;

   Result := Z_STREAM_ERROR;
End;
//-------------------------------------------------------------

Function inflate_blocks(Var s: inflate_blocks_state; Var z: ztv_stream;
   R: _int): _int;
Label
   start_btree, start_dtree,
      start_blkdone, start_dry,
      start_codes;

Var
   t: _uInt;                             { temporary storage }
   b: u_long;                            { bit buffer }
   k: _uInt;                             { bits in bit buffer }
   p: _pBytef;                           { input data pointer }
   n: _uInt;                             { bytes available there }
   q: _pBytef;                           { output window write pointer }
   m: _uInt;                             { bytes to end of window or read pointer }

   bl, bd: {u_long; //}_uInt; //v6.1
   tl, td: phuft;
   H: phuft;
   i, j, C: _uInt;
   cs: pInflate_codes_state;

   { Tables for deflate from PKZIP's appnote.txt. }
Begin
   p := z.next_in;
   n := z.avail_in;
   b := s.bitb;
   k := s.bitk;
   q := s.Write;

   If ptr2int(q) < ptr2int(s.Read) Then
      m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
   Else
      m := _uInt(ptr2int(s.zend) - ptr2int(q));

   While True Do
      Case s.mode Of
         ZTYPE:
            Begin                 
               While (k < 3) Do
               Begin
                  If (n <> 0) Then
                     R := Z_OK
                  Else
                  Begin
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;
                  Dec(n);
                  b := b Or (u_long(p^) Shl k);
                  inc(p);
                  inc(k, 8);
               End;

               t := _uInt(b) And 7; // v6.1
               //t := b And 7;

               s.last := Boolean(t And 1);
               Case (t Shr 1) Of
                  0:                    { stored }
                     Begin
                     	// DumpBits(3)
                        b := b Shr 3;
                        Dec(k, 3);

                        t := k And 7;   { go to byte boundary }

                        // DumpBits(t);
                        b := b Shr t;
                        Dec(k, t);

                        s.mode := LENS; { get length of stored block }
                     End;
                  1:                    { fixed }
                     Begin
                        Begin
                           inflate_trees_fixed(bl, bd, tl, td, z);
                           s.sub.decode.CODES := inflate_codes_new(bl, bd, tl, td, z);

                           If (s.sub.decode.CODES = Z_NULL) Then
                           Begin
                              R := Z_MEM_ERROR;
                              s.bitb := b;
                              s.bitk := k;
                              z.avail_in := n;
                              inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                              z.next_in := p;
                              s.Write := q;
                              Result := inflate_flush(s, z, R);
                              Exit;
                           End;
                        End;

                        b := b Shr 3;
                        Dec(k, 3);

                        s.mode := CODES;
                     End;
                  2:                    { dynamic }
                     Begin
                        b := b Shr 3;
                        Dec(k, 3);

                        s.mode := TABLE;
                     End;
                  3:
                     Begin              { illegal }
                        b := b Shr 3;
                        Dec(k, 3);

                        s.mode := BLKBAD;
                        //invalid block type
                        R := Z_DATA_ERROR;
                        s.bitb := b;
                        s.bitk := k;
                        z.avail_in := n;
                        inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                        z.next_in := p;
                        s.Write := q;
                        Result := inflate_flush(s, z, R);
                        Exit;
                     End;
               End;
            End;
         LENS:
            Begin
               While (k < 32) Do
               Begin
                  If (n <> 0) Then
                     R := Z_OK
                  Else
                  Begin
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;
                  Dec(n);
                  b := b Or (u_long(p^) Shl k);
                  inc(p);
                  inc(k, 8);
               End;

               If (((Not b) Shr 16) And $FFFF) <> (b And $FFFF) Then
               Begin
                  s.mode := BLKBAD;
                  //invalid stored block lengths
                  R := Z_DATA_ERROR;

                  // update pointers and return
                  s.bitb := b;
                  s.bitk := k;
                  z.avail_in := n;
                  inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                  z.next_in := p;
                  s.Write := q;
                  Result := inflate_flush(s, z, R);
                  Exit;
               End;

               s.sub.left := _uInt(b) And $FFFF;
               k := 0;
               b := 0;                  { dump bits }

               If s.sub.left <> 0 Then
                  s.mode := STORED
               Else
                  If s.last Then
                     s.mode := DRY
                  Else
                     s.mode := ZTYPE;
            End;
         STORED:
            Begin
               If (n = 0) Then
               Begin
                  { update pointers and return }
                  s.bitb := b;
                  s.bitk := k;
                  z.avail_in := n;
                  inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                  z.next_in := p;
                  s.Write := q;
                  Result := inflate_flush(s, z, R);
                  Exit;
               End;
               If (m = 0) Then
               Begin
                  If (q = s.zend) And (s.Read <> s.window) Then
                  Begin
                     q := s.window;
                     If ptr2int(q) < ptr2int(s.Read) Then
                        m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                     Else
                        m := _uInt(ptr2int(s.zend) - ptr2int(q));
                  End;

                  If (m = 0) Then
                  Begin
                     {FLUSH}
                     s.Write := q;
                     R := inflate_flush(s, z, R);
                     q := s.Write;
                     If ptr2int(q) < ptr2int(s.Read) Then
                        m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                     Else
                        m := _uInt(ptr2int(s.zend) - ptr2int(q));

                     If (q = s.zend) And (s.Read <> s.window) Then
                     Begin
                        q := s.window;
                        If ptr2int(q) < ptr2int(s.Read) Then
                           m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
                        Else
                           m := _uInt(ptr2int(s.zend) - ptr2int(q));
                     End;

                     If (m = 0) Then
                     Begin
                        s.bitb := b;
                        s.bitk := k;
                        z.avail_in := n;
                        inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                        z.next_in := p;
                        s.Write := q;
                        Result := inflate_flush(s, z, R);
                        Exit;
                     End;
                  End;
               End;
               R := Z_OK;

               t := s.sub.left;
               If (t > n) Then
                  t := n;
               If (t > m) Then
                  t := m;

               CopyMem(@p^, @q^, t);
               inc(p, t);
               Dec(n, t);
               inc(q, t);
               Dec(m, t);
               Dec(s.sub.left, t);
               If (s.sub.left = 0) Then
               Begin
                  If s.last Then
                     s.mode := DRY
                  Else
                     s.mode := ZTYPE;
               End;
            End;
         TABLE:
            Begin
               While (k < 14) Do
               Begin
                  If (n <> 0) Then
                     R := Z_OK
                  Else
                  Begin
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;
                  Dec(n);
                  b := b Or (u_long(p^) Shl k);
                  inc(p);
                  inc(k, 8);
               End;

               t := _uInt(b) And $3FFF; 
               //t := b And $3FFF;	//v6.1

               s.sub.trees.TABLE := t;
               If (Not is64Bit) Then
                  If ((t And $1F) > 29) Or (((t Shr 5) And $1F) > 29) Then
                  Begin
                     s.mode := BLKBAD;
                     {too many length or distance symbols}
                     R := Z_DATA_ERROR;
                     { update pointers and return }
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;
               t := 258 + (t And $1F) + ((t Shr 5) And $1F);
               s.sub.trees.blens := PuIntArray(ZALLOC(z, t, SizeOf(_uInt)));
               //s.sub.trees.blens := PuIntArray(ZALLOC(z, t, SizeOf(u_long)));	// v6.1
               If (s.sub.trees.blens = Z_NULL) Then
               Begin
                  R := Z_MEM_ERROR;
                  { update pointers and return }
                  s.bitb := b;
                  s.bitk := k;
                  z.avail_in := n;
                  inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                  z.next_in := p;
                  s.Write := q;
                  Result := inflate_flush(s, z, R);
                  Exit;
               End;
               b := b Shr 14;
               Dec(k, 14);

               s.sub.trees.Index := 0;
               s.mode := BTREE;
               { fall trough case is handled by the while }
               { try GOTO for speed - Nomssi }
               Goto start_btree;
            End;
         BTREE:
            Begin
               start_btree:
               While (s.sub.trees.Index < 4 + (s.sub.trees.TABLE Shr 10)) Do
               Begin
                  While (k < 3) Do
                  Begin
                     If (n <> 0) Then
                        R := Z_OK
                     Else
                     Begin
                        s.bitb := b;
                        s.bitk := k;
                        z.avail_in := n;
                        inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                        z.next_in := p;
                        s.Write := q;
                        Result := inflate_flush(s, z, R);
                        Exit;
                     End;
                     Dec(n);
                     b := b Or (u_long(p^) Shl k);
                     inc(p);
                     inc(k, 8);
                  End;

                  s.sub.trees.blens^[border[s.sub.trees.Index]] := _uInt(b) And 7;
                  //s.sub.trees.blens^[border[s.sub.trees.Index]] := b And 7;	// v6.1
                  inc(s.sub.trees.Index);
                  b := b Shr 3;
                  Dec(k, 3);
               End;
               While (s.sub.trees.Index < 19) Do
               Begin
                  s.sub.trees.blens^[border[s.sub.trees.Index]] := 0;
                  inc(s.sub.trees.Index);
               End;
               s.sub.trees.bb := 7;
               t := inflate_trees_bits(s.sub.trees.blens^, s.sub.trees.bb,
                  s.sub.trees.tb, s.hufts^, z);
               If (t <> Z_OK) Then
               Begin
                  ZFREE(z, s.sub.trees.blens);
                  R := t;
                  If (R = Z_DATA_ERROR) Then
                     s.mode := BLKBAD;
                  { update pointers and return }
                  s.bitb := b;
                  s.bitk := k;
                  z.avail_in := n;
                  inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                  z.next_in := p;
                  s.Write := q;
                  Result := inflate_flush(s, z, R);
                  Exit;
               End;
               s.sub.trees.Index := 0;
               s.mode := dtree;
               { fall through again }
               Goto start_dtree;
            End;
         dtree:
            Begin
               start_dtree:
               While True Do
               Begin
                  t := s.sub.trees.TABLE;
                  If Not (s.sub.trees.Index < 258 +
                     (t And $1F) + ((t Shr 5) And $1F)) Then
                     break;
                  t := s.sub.trees.bb;
                  While (k < t) Do
                  Begin
                     If (n <> 0) Then
                        R := Z_OK
                     Else
                     Begin
                        s.bitb := b;
                        s.bitk := k;
                        z.avail_in := n;
                        inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                        z.next_in := p;
                        s.Write := q;
                        Result := inflate_flush(s, z, R);
                        Exit;
                     End;
                     Dec(n);
                     b := b Or (u_long(p^) Shl k);
                     inc(p);
                     inc(k, 8);
                  End;

                  H := s.sub.trees.tb;
                  inc(H, _uInt(b) And inflate_mask[t]);
                  t := H^.b;
                  C := H^.v_n;

                  If (C < 16) Then
                  Begin
                     b := b Shr t;
                     Dec(k, t);

                     s.sub.trees.blens^[s.sub.trees.Index] := C;
                     inc(s.sub.trees.Index);
                  End
                  Else                  { c = 16..18 }
                  Begin
                     If C = 18 Then
                     Begin
                        i := 7;
                        j := 11;
                     End
                     Else
                     Begin
                        i := C - 14;
                        j := 3;
                     End;
                     While (k < t + i) Do
                     Begin
                        If (n <> 0) Then
                           R := Z_OK
                        Else
                        Begin
                           s.bitb := b;
                           s.bitk := k;
                           z.avail_in := n;
                           inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                           z.next_in := p;
                           s.Write := q;
                           Result := inflate_flush(s, z, R);
                           Exit;
                        End;
                        Dec(n);
                        b := b Or (u_long(p^) Shl k);
                        inc(p);
                        inc(k, 8);
                     End;

                     b := b Shr t;
                     Dec(k, t);

                     inc(j, _uInt(b) And inflate_mask[i]);
                     b := b Shr i;
                     Dec(k, i);

                     i := s.sub.trees.Index;
                     t := s.sub.trees.TABLE;
                     If (i + j > 258 + (t And $1F) + ((t Shr 5) And $1F)) Or
                        ((C = 16) And (i < 1)) Then
                     Begin
                        ZFREE(z, s.sub.trees.blens);
                        s.mode := BLKBAD;
                        //invalid bit length repeat
                        R := Z_DATA_ERROR;
                        { update pointers and return }
                        s.bitb := b;
                        s.bitk := k;
                        z.avail_in := n;
                        inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                        z.next_in := p;
                        s.Write := q;
                        Result := inflate_flush(s, z, R);
                        Exit;
                     End;
                     If C = 16 Then
                        C := s.sub.trees.blens^[i - 1]
                     Else
                        C := 0;
                     Repeat
                        s.sub.trees.blens^[i] := C;
                        inc(i);
                        Dec(j);
                     Until (j = 0);
                     s.sub.trees.Index := i;
                  End;
               End;

               s.sub.trees.tb := Z_NULL;

               Begin
                  bl := 7; // 9; v4.8.5 changed { must be <= 9 for lookahead assumptions }
                  bd := 6;                      { must be <= 9 for lookahead assumptions }

                  t := s.sub.trees.TABLE;
                  t :=
                  	inflate_trees_dynamic(257 + (t And $1F),
                     	1 + ((t Shr 5) And $1F),
                     	s.sub.trees.blens^, bl, bd, tl, td, s.hufts^, z);

                  ZFREE(z, s.sub.trees.blens);
                  If (t <> Z_OK) Then
                  Begin
                     If (t = _uInt(Z_DATA_ERROR)) Then
                        s.mode := BLKBAD;
                     R := t;
                     { update pointers and return }
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;

                  { c renamed to cs }
                  cs := inflate_codes_new(bl, bd, tl, td, z);
                  If (cs = Z_NULL) Then
                  Begin
                     R := Z_MEM_ERROR;
                     { update pointers and return }
                     s.bitb := b;
                     s.bitk := k;
                     z.avail_in := n;
                     inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                     z.next_in := p;
                     s.Write := q;
                     Result := inflate_flush(s, z, R);
                     Exit;
                  End;
                  s.sub.decode.CODES := cs;
               End;
               s.mode := CODES;
               Goto start_codes;
            End;
         CODES:
            Begin
start_codes:
               { update pointers }
               s.bitb := b;
               s.bitk := k;
               z.avail_in := n;
               inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
               z.next_in := p;
               s.Write := q;

               R := inflate_codes(s, z, R);
               If (R <> Z_STREAM_END) Then
               Begin
                  Result := inflate_flush(s, z, R);
                  Exit;
               End;

               R := Z_OK;
               inflate_codes_free(s.sub.decode.CODES, z);

               { load local pointers }
               p := z.next_in;
               n := z.avail_in;
               b := s.bitb;
               k := s.bitk;
               q := s.Write;
               If ptr2int(q) < ptr2int(s.Read) Then
                  m := _uInt(ptr2int(s.Read) - ptr2int(q) - 1)
               Else
                  m := _uInt(ptr2int(s.zend) - ptr2int(q));
               If (Not s.last) Then
               Begin
                  s.mode := ZTYPE;
                  Continue;             { break for switch statement in C-code }
               End;
               If (k > 7) Then          { return unused byte, if any }
               Begin
                  Dec(k, 8);
                  inc(n);
                  Dec(p);               { can always return one }
               End;
               s.mode := DRY;
               Goto start_dry;
            End;
         DRY:
            Begin
               start_dry:
               {FLUSH}
               s.Write := q;
               R := inflate_flush(s, z, R);
               q := s.Write;

               { not needed anymore, we are done:
               if ptr2int(q) < ptr2int(s.read) then
                 m := _uInt(ptr2int(s.read)-ptr2int(q)-1)
               else
                 m := _uInt(ptr2int(s.zend)-ptr2int(q));
               }

               If (s.Read <> s.Write) Then
               Begin
                  { update pointers and return }
                  s.bitb := b;
                  s.bitk := k;
                  z.avail_in := n;
                  inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
                  z.next_in := p;
                  s.Write := q;
                  Result := inflate_flush(s, z, R);
                  Exit;
               End;
               s.mode := BLKDONE;
               Goto start_blkdone;
            End;
         BLKDONE:
            Begin
               start_blkdone:
                R := Z_STREAM_END;
               { update pointers and return }
               s.bitb := b;
               s.bitk := k;
               z.avail_in := n;
               inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
               z.next_in := p;
               s.Write := q;
               Result := inflate_flush(s, z, R);
               Exit;
            End;
         BLKBAD:
            Begin
               R := Z_DATA_ERROR;
               { update pointers and return }
               s.bitb := b;
               s.bitk := k;
               z.avail_in := n;
               inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
               z.next_in := p;
               s.Write := q;
               Result := inflate_flush(s, z, R);
               Exit;
            End;
      Else
         Begin
            R := Z_STREAM_ERROR;
            { update pointers and return }
            s.bitb := b;
            s.bitk := k;
            z.avail_in := n;
            inc(z.total_in, ptr2int(p) - ptr2int(z.next_in));
            z.next_in := p;
            s.Write := q;
            Result := inflate_flush(s, z, R);
            Exit;
         End;
      End;                              { Case s.mode of }

End;
//-------------------------------------------------------------

Function _inflate(Var z: ztv_stream; f: _int): _int;
Var
   R: _int;
   b: _uInt;
Begin
   If (z.state = Z_NULL) Or (z.next_in = Z_NULL) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   If f = Z_FINISH Then
      f := Z_BUF_ERROR
   Else
      f := Z_OK;

   R := Z_BUF_ERROR;

   While True Do
      Case (z.state^.mode) Of
         BLOCKS:
            Begin
               R := inflate_blocks(z.state^.BLOCKS^, z, R);
               //If (R = Z_BUF_ERROR) Then
               //	ShowMessage('Here');

               If (R = Z_DATA_ERROR) Then
               Begin
                  z.state^.mode := zBAD;
                  z.state^.sub.marker := 0; { can try inflateSync }
                  Continue;
               End;

               If (R = Z_OK) Then
                  R := f;

               If (R <> Z_STREAM_END) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;

               inflate_blocks_reset(z.state^.BLOCKS^, z, @z.state^.sub.Check.was);

               If (z.state^.nowrap) Then
               Begin
                  z.state^.mode := DONE;
                  Continue;
               End;

               z.state^.mode := CHECK4;
            End;
         CHECK4:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;
               Dec(z.avail_in);
               inc(z.total_in);
               z.state^.sub.Check.need := u_long(z.next_in^) Shl 24;
               inc(z.next_in);

               z.state^.mode := CHECK3;
            End;
         CHECK3:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;
               Dec(z.avail_in);
               inc(z.total_in);
               inc(z.state^.sub.Check.need, u_long(z.next_in^) Shl 16);
               inc(z.next_in);

               z.state^.mode := CHECK2;
            End;
         CHECK2:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;
               Dec(z.avail_in);
               inc(z.total_in);
               inc(z.state^.sub.Check.need, u_long(z.next_in^) Shl 8);
               inc(z.next_in);

               z.state^.mode := CHECK1;
            End;
         CHECK1:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;
               Dec(z.avail_in);
               inc(z.total_in);
               inc(z.state^.sub.Check.need, u_long(z.next_in^));
               inc(z.next_in);

               If (z.state^.sub.Check.was <> z.state^.sub.Check.need) Then
               Begin
                  z.state^.mode := zBAD;
                  //incorrect data check
                  z.state^.sub.marker := 5; { can't try inflateSync }
                  Continue;
               End;

               z.state^.mode := DONE;
            End;
         DONE:
            Begin
               Result := Z_STREAM_END;
               Exit;
            End;
         method:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;
               Dec(z.avail_in);
               inc(z.total_in);
               z.state^.sub.method := z.next_in^;
               inc(z.next_in);

               If ((z.state^.sub.method And $0F) <> Z_DEFLATED) Then
               Begin
                  z.state^.mode := zBAD;
                  //unknown compression method
                  z.state^.sub.marker := 5; { can't try inflateSync }
                  Continue;
               End;

               If ((z.state^.sub.method Shr 4) + 8 > z.state^.wbits) Then
               Begin
                  z.state^.mode := zBAD;
                  //invalid window size
                  z.state^.sub.marker := 5; { can't try inflateSync }
                  Continue;
               End;

               z.state^.mode := FLAG;
            End;
         FLAG:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;                  {}
               Dec(z.avail_in);
               inc(z.total_in);
               b := z.next_in^;
               inc(z.next_in);

               If (((z.state^.sub.method Shl 8) + b) Mod 31) <> 0 Then {% mod ?}
               Begin
                  z.state^.mode := zBAD;
                  //incorrect header check
                  z.state^.sub.marker := 5; { can't try inflateSync }
                  Continue;
               End;

               If ((b And PRESET_DICT) = 0) Then
               Begin
                  z.state^.mode := BLOCKS;
                  Continue;
               End;

               z.state^.mode := DICT4;
            End;
         DICT4:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;
               Dec(z.avail_in);
               inc(z.total_in);
               z.state^.sub.Check.need := u_long(z.next_in^) Shl 24;
               inc(z.next_in);

               z.state^.mode := DICT3;
            End;
         DICT3:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;
               Dec(z.avail_in);
               inc(z.total_in);
               inc(z.state^.sub.Check.need, u_long(z.next_in^) Shl 16);
               inc(z.next_in);

               z.state^.mode := DICT2;
            End;
         DICT2:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               R := f;
               Dec(z.avail_in);
               inc(z.total_in);
               inc(z.state^.sub.Check.need, u_long(z.next_in^) Shl 8);
               inc(z.next_in);

               z.state^.mode := DICT1;
            End;
         DICT1:
            Begin
               If (z.avail_in = 0) Then
               Begin
                  Result := R;
                  Exit;
               End;

               { r := f; }
               Dec(z.avail_in);
               inc(z.total_in);
               inc(z.state^.sub.Check.need, u_long(z.next_in^));
               inc(z.next_in);

               z.adler := z.state^.sub.Check.need;
               z.state^.mode := DICT0;
               Result := Z_NEED_DICT;
               Exit;
            End;
         DICT0:
            Begin
               z.state^.mode := zBAD;
               //need dictionary
               z.state^.sub.marker := 0; { can try inflateSync }
               Result := Z_STREAM_ERROR;
               Exit;
            End;
         zBAD:
            Begin
               Result := Z_DATA_ERROR;
               Exit;
            End;
      Else
         Begin
            Result := Z_STREAM_ERROR;
            Exit;
         End;
      End;

End;
//-------------------------------------------------------------

{Procedure inflate_set_dictionary(Var s: inflate_blocks_state;
   Const d: Array Of Byte;              // dictionary
   n: _uInt);                            // dictionary length
Begin
   CopyMem(@d, @s.window, n);
   s.Write := s.window;
   inc(s.Write, n);
   s.Read := s.Write;
End;}
//-------------------------------------------------------------

{Function inflateSetDictionary(Var z: ztv_stream; dictionary: _pBytef;
	dictLength: _uInt): _int;
Var
   Length: _uInt;
Begin
   Length := dictLength;

   If (z.state = Z_NULL) Or (z.state^.mode <> DICT0) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   If (_adler(u_long(1), dictionary, dictLength) <> z.adler) Then
   Begin
      Result := Z_DATA_ERROR;
      Exit;
   End;

   z.adler := u_long(1);

   If (Length >= (_uInt(1) Shl z.state^.wbits)) Then
   Begin
      Length := (1 Shl z.state^.wbits) - 1;
      inc(dictionary, dictLength - Length);
   End;

   inflate_set_dictionary(z.state^.BLOCKS^, dictionary^, Length);
   z.state^.mode := BLOCKS;
   Result := Z_OK;
End;}
//-------------------------------------------------------------

{Function inflateSync( Var z: ztv_stream ): _int;
Const
   mark: Packed Array[0..3] Of Byte = ( 0, 0, $FF, $FF );
Var
   n: _uInt;                             // number of bytes to look at
   p: _pBytef;                          // pointer to bytes
   m: _uInt;                             // number of marker bytes found in a row
   r, w: u_long;                        // temporaries to save total_in and total_out
Begin
   If ( z.state = Z_NULL ) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;

   If ( z.state^.mode <> zBAD ) Then
   Begin
      z.state^.mode := zBAD;
      z.state^.sub.marker := 0;
   End;

   n := z.avail_in;
   If ( n = 0 ) Then
   Begin
      Result := Z_BUF_ERROR;
      Exit;
   End;

   p := z.next_in;
   m := z.state^.sub.marker;

   // search
   While ( n <> 0 ) And ( m < 4 ) Do
   Begin
      If ( p^ = mark[m] ) Then
         Inc( m )
      Else If ( p^ <> 0 ) Then
         m := 0
      Else
         m := 4 - m;
      Inc( p );
      Dec( n );
   End;

   // restore
   Inc( z.total_in, ptr2int( p ) - ptr2int( z.next_in ) );
   z.next_in := p;
   z.avail_in := n;
   z.state^.sub.marker := m;

   If ( m <> 4 ) Then
   Begin
      Result := Z_DATA_ERROR;
      Exit;
   End;

   r := z.total_in;
   w := z.total_out;
   inflateReset( z );
   z.total_in := r;
   z.total_out := w;
   z.state^.mode := BLOCKS;
   Result := Z_OK;
End;}
//-------------------------------------------------------------

{Function inflate_blocks_sync_point( Var s: inflate_blocks_state ): _int;
Begin
   Result := _int( s.mode = LENS );
End;}
//-------------------------------------------------------------
//  returns true if inflate is currently at the end of a block generated
//  by Z_SYNC_FLUSH or Z_FULL_FLUSH. This function is used by one PPP
//  implementation to provide an additional safety check. PPP uses Z_SYNC_FLUSH
//  but removes the length bytes of the resulting empty stored block. When
//  decompressing, PPP checks that at the end of input packet, inflate is
//  waiting for these length bytes.

{Function inflateSyncPoint( Var z: ztv_stream ): _int;
Begin
   If ( z.state = Z_NULL ) Or ( z.state^.BLOCKS = Z_NULL ) Then
   Begin
      Result := Z_STREAM_ERROR;
      Exit;
   End;
   Result := inflate_blocks_sync_point( z.state^.BLOCKS^ );
End;}
//-------------------------------------------------------------
(* Returns CRC of decompressed data *)
// IMPORTANT: this function will not overwrite an existing file.  It
// is the developers responsibility to delete these files prior to
// calling this function.

Function ztvDecompress_StreamToFile(Stream: TStream32; FileName: String): u_long;
Var
   cStream: TStream32;
   FileStream: TFileStream32;
Begin
   Result := 0;
   If FileExists(FileName) Then Exit;
   cStream := TMemoryStream32.Create();
   Try
      Result := ztvDecompress_StreamToStream(Stream, cStream, Stream.size);
      If cStream.size > 0 Then
      Begin
         cStream.Position := 0;

         FileStream :=
         	TFileStream32.Create(FileName, fmCreate Or fmShareExclusive);

         //FileStream.CancelCallBackProc := Nil;
         //FileStream.ProgressCallBackProc := Nil;

   		If (FileStream.Handle < 0) Then Exit;

         Try
            //FileStream.CopyFrom(cStream, 0, Nil, Nil);
            FileStream.CopyFrom(cStream, 0);
         Finally
            FileStream.Free();
         End;
      End;
   Finally
      cStream.Free();
   End;
End;
//-------------------------------------------------------------

// Notes:
// 1. Returns CRC of decompressed data
// 2. if the resulting crc does not match the stored crc (s.crc),
// the returned crc = CRC_MASK, indicating a decompress failure.
// 3. TClipBoardSig is defined in ztvBase.pas
// 4. ClipBoard_FillerBytes: see ztvDeflate.pas
Function ztvDecompress_StreamFromClipboard(Stream: TStream32): u_long;
Var
   s: TClipBoardSig;
   cStream: TMemoryStream32;

   Function Unstore: u_long;
   Var
      Size: Int64;
      Buffer: Pointer;
    	MemSize: Integer;
      BytesRead: Cardinal;
   Begin
   	Result := CRC_MASK;

      Size := cStream.Size - cStream.Position - ClipBoard_FillerBytes;
      If Size > WSIZE{4096} Then
         MemSize := WSIZE
      Else
         MemSize := Size;

      GetMem(Buffer, MemSize + 1);
      Try
         While Size > 0 Do
         Begin

            BytesRead := cStream.Read(Buffer^, MemSize);
				Crc32_buf(@Buffer^{@Buffer[0]}, BytesRead, Result);
            If BytesRead = 0 Then
               Break;

            Dec(Size, BytesRead);
            //ProgressPosition := ProgressPosition - BytesRead;

            Stream.Write(Buffer^{Buffer[0]}, BytesRead);
            //If IR.UnpackedSize > Size Then
            //   doBranchProgress(IR.UnpackedSize - Size, IR.UnpackedSize, fTotalPackedSize);
            //If Cancel Then Break;

            // use the following block, instead of the min function... the min
            // function fails with files > 4 gig.
            If Size > WSIZE Then
               MemSize := WSIZE
            Else
               MemSize := Size;
         End;
      Finally
         FreeMem(Buffer);
         Stream.Position := 0;
      End;
   End;
Begin
   Result := CRC_MASK;
   cStream := TMemoryStream32.Create();
   Try
      If Not CopyStreamFromClipboard(cStream) Then
         Exit;

      cStream.Read(s, SizeOf(s));

      If s.Signature = LOCAL_CUST_HEADER_SIGNATURE Then
      Begin
      	If s.dt = dtDeflateS Then		// (sb = 0) = stored data
         	Result := Unstore()
         Else
         	Result :=
            	ztvDecompress_StreamToStream(cStream, Stream,
               	cStream.size - SizeOf(s))
      End;
   Finally

      If Result <> s.crc Then
      	Result := CRC_MASK;

      cStream.Free;
   End;
End;
//-------------------------------------------------------------
(* Returns CRC of decompressed data *)
(* revised v6.8 *)

Function ztvDecompress_StreamToStream(InStream, OutStream: TStream32;
	InBytes: _int): u_long;
Var
   Cancel: Boolean;
   Count: Integer;
   Buffer: Pointer;
   Stream: TDecompressStream;
Begin
	Crc32Val := CRC_MASK;

   Stream := TDecompressStream.Create(InStream, _MAX_WBITS);
   Try
      Stream.FZRec.cb.pCancel := @Cancel;
      Stream.FZRec.cb.Protect := False;

      GetMem(Buffer, WSIZE);
      Try
         While True Do
         Begin
            If Cancel Then break;
            Try
               Count := Stream.Read(Buffer^, WSIZE);
               If Stream.FZRec.Total_out > 0 Then
               Begin
               	OutStream.Write(Buffer^, Stream.FZRec.Total_out);
						Crc32_buf(Buffer, Stream.FZRec.Total_out, Crc32Val);
                  Stream.FZRec.Total_out := 0;
                  //ProgressPosition := ProgressPosition - Count;
                  //FileProgressPos := FileProgressPos - Count;
               End Else
               	Break;

               //doBranchProgress(FileSize - FileProgressPos, FileSize,
               //   fTotalUnpackedSize);

               If (Count <> -5) And (Count < 1) Then
                 break;

            Except
               On EWriteError Do
                  Raise;
            Else
            End;
         End;
      Finally
         FreeMem(Buffer);
      End;
   Finally
   	Result := Crc32Val;
      Stream.Free();
   End;
End;
//-------------------------------------------------------------

(* Returns CRC of decompressed data *)
Function ztvDecompress_BufToBuf(Const inbuf: Pointer; InBytes: _int;
   Var outbuf: Pointer; Var outBytes: _int; OutEstimate: _int): u_long;
Var
   p: Pointer;
   strm: ztv_stream;
   BufInc: _int;
   Cancel: Boolean;
Begin
   FillChar(strm, SizeOf(strm), 0);
   strm.ZALLOC := @ztvAllocMem;
   strm.ZFREE := @ztvFreeMem;
   strm.cb.CRC := CRC_MASK;
   Try
      BufInc := (InBytes + 255) And Not 255;
      If OutEstimate = 0 Then
         outBytes := BufInc
      Else
         outBytes := OutEstimate;

      GetMem(outbuf, outBytes);
      Try
         strm.next_in := inbuf;
         strm.avail_in := InBytes;
         strm.next_out := outbuf;
         strm.avail_out := outBytes;
         strm.cb.pCancel := @Cancel;
         inflateInit_(@strm, _MAX_WBITS, SizeOf(strm));	// v4.1.7
         Try
         	// v4.6.2 revised.  Changed <> to <.
            //While _inflate(strm, Z_FINISH) < Z_STREAM_END Do
            While _inflate(strm, Z_FINISH) = Z_OK Do	// v6.5.2
            Begin
               p := outbuf;
               inc(outBytes, BufInc);
               ReallocMem(outbuf, outBytes);
               strm.next_out := _pBytef(_int(outbuf) + (_int(strm.next_out) - _int(p)));
               strm.avail_out := BufInc;
               If Cancel Then break;
            End;
         Finally
            inflateEnd(strm);	// v4.1.7
         End;
         ReallocMem(outbuf, strm.total_out);
         outBytes := strm.total_out;
   		Crc32_buf(outbuf, outBytes, strm.cb.CRC);
      Except
         FreeMem(outbuf);
         Raise
      End;
   Finally
      Result := strm.cb.CRC;
   End;
End;
//-------------------------------------------------------------

Function ztvDecompress_String_Method2(s: String; StrLength: Integer): String;
Var
	CompressedStrm: TStream32;
   BufSize,
   	Count: _int;
   Buffer: Pointer;
   Stream: TDecompressStream;
Begin
   Result := '';

   If (s = '') Or (StrLength = 0) Then Exit;

	CompressedStrm := TMemoryStream32.Create();
   CompressedStrm.Write(s[1], StrLength);
   CompressedStrm.Seek(0, soBeginning);

   Try
      Stream := TDecompressStream.Create(CompressedStrm, MAX_WBITS);

      Try
      	Stream.FZRec.cb.pCancel := Nil;
   		Stream.FZRec.cb.Protect := False;
         Stream.Seek(0, soBeginning);

         BufSize := WSIZE;
			is64Bit := False;

         GetMem(Buffer, BufSize);
         Try
            Try
               Count := Stream.Read(Buffer^, BufSize);
               If Count > 0 Then ;
               If Stream.FZRec.total_out > 0 Then
               Begin
                  SetLength(Result, Stream.FZRec.total_out);
						FillChar(Result[1], Stream.FZRec.total_out, 0);
                  Move(Buffer^, Result[1], Stream.FZRec.total_out);
               End;
            Except
               On EWriteError Do
                  Raise;
            End;
         Finally
            FreeMem(Buffer);
         End;
      Finally
         Stream.Free();
      End;
   Finally
   	CompressedStrm.Free();
   End;
End;
//-------------------------------------------------------------

(* Returns decompressed string *)
Function ztvDecompress_String_Method1(s: String): String;
Var
	Len,
   	i,
   	size: _int;
   Buffer: Pointer;
   BufPtr: _pBytef;
Begin
	Len := Length(s);

   BufPtr := Buffer;
   For i := 0 To Len - 1 Do
   Begin
      If BufPtr^ = 255 Then
         BufPtr^ := 0;
      Inc(BufPtr);
   End;

   ztvDecompress_BufToBuf(PChar(s), Length(s), Buffer, size, 0);
   Try
      SetLength(Result, size);
   	Move( Buffer^, Result[1], Size );
   Finally
      FreeMem(Buffer);
   End;
End;
//-------------------------------------------------------------

End.
