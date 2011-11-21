Unit ztvInflate64;

Interface

Uses
   Windows,
   Classes,
   SysUtils,
   ztvBase,
   ztvStreams,
   ztvConsts;

Type
   TInflateProc = Packed Record
      RB: TReadBlock;
      WB: TWriteBlock;
      PP: TProgress;
      pProgressPos: pWord64;
      pCancel: pBoolean;
   End;

   //pWindowOffset = ^WindowOffset;
   //WindowOffset = Packed Record
   //   zWindow: Pointer;
   //   zEnd: Pointer;
   //   p: Pointer;
   //End;
   //
   //Var
   //	WindowPtr: pWindowOffset;

Var
   InflateProc: TInflateProc;

Function Inflate(Inf: TStream32; Var Outf: TStream32;
   IR: TInflateRec; ABIT: Byte; InflateProc: TInflateProc): Boolean;
Function Explode(Inf: TStream32; Var Outf: TStream32;
   IR: TInflateRec; ABIT: Byte; InflateProc: TInflateProc): Boolean;

Implementation

Uses
   Dialogs,
   ztvGbls,
   ztvErrMsgs;

Const
   InvalidCode = 99;
   NonSimpleLookup = 0;
   EndOfBlock = 31;
   ValidCode = 32;

Const
   Border: Array[0..18] Of Byte =       { Order of the bit length code lengths }
   (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

   (* Tables for deflate from PKZIP's appnote.txt. *)
   cplens64: Array[0..30] Of Word =     //Def64
   	(3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
      35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 3, 0, 0);
   cplens: Array[0..30] Of Word =       (* Copy lengths for literal codes 257..285 *)
   	(3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
      35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);

   (* note: see note #13 above about the 258 in this list. *)
   cpdist64: Array[0..31] Of Word = //Def64    { Copy offsets for distance codes 0..31 }
   (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257,
      385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289,
      16385, 24577, 32769, 49153);
   cpdist: Array[0..29] Of Word =       (* Copy offsets for distance codes 0..29 *)
   (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257,
      385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289,
      16385, 24577);

   cplext64: Array[0..30] Of Word {Byte} = //Def64
   (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
      3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 16, InvalidCode, InvalidCode);
   cplext: Array[0..30] Of Word {Byte} = (* Extra bits for literal codes 257..285 *)
   (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
      3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, InvalidCode, InvalidCode);

   cpdext64: Array[0..31] Of Word {Byte} = //Def64    { Extra bits for distance codes }
   (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
      7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13,
      14, 14);
   cpdext: Array[0..29] Of Word {Byte} = (* Extra bits for distance codes *)
   (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
      7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13);

   {b and mask_bits[i] gets lower i bits out of i}
   mask_bits: Array[0..16] Of Word =
      ($0000, $0001, $0003, $0007, $000F, $001F, $003F, $007F,
      $00FF, $01FF, $03FF, $07FF, $0FFF, $1FFF, $3FFF, $7FFF,
      $FFFF);

Var
   ReadProc: TReadBlock;
   WriteProc: TWriteBlock;
   ProgressProc: TProgress;
   pProgressPos: pWord64;
   pCancel: pBoolean;
   Bytes_To_Go: Int64;

   Infile: TStream32;
   Outfile: TStream32;
   InflateRec: TInflateRec;

   hufttype: Word;     // explode
   AdditionalBitsInTable: Byte;

Const                                   {codes returned by huft_build}
   huft_complete = 0;                   {complete tree}
   huft_incomplete = 1;                 {incomplete tree <- sufficient in some cases!}
   huft_error = 2;                      {bad tree constructed}
   huft_outofmem = 3;                   {not enough memory}

Const
   unzip_Ok = 0;
   unzip_WriteErr = -2;
   unzip_ReadErr = -3;
   unzip_ZipFileErr = -4;
   unzip_UserAbort = -5;

Const
   MaxMax = 31 * 1024;
   INBUFSIZ = 32768; //High(Word); //1024 * 4;   {Size of input buffer (4kb) }
   lbits: Integer = 9;
   dbits: Integer = 6;
   b_max = 16;
   n_max = 288;
   BMAX = 16;

Type
   PushList = ^ushlist;
   ushlist = Array[0..MaxMax] Of ush;   {only pseudo-size!!}
   PioBuf = ^Iobuf;
   Iobuf = Array[0..INBUFSIZ - 1] Of Byte;

Type
   pphuft = ^phuft;
   phuft = ^huft;
   phuftlist = ^HuftList;
   huft = Record
      e{exop},                        	 { number of extra bits or operation 				}
      b{bits}: Byte;                    { number of bits in this code or subcode 		}
      //pad : uInt;                     { pad structure to a power of 2 (4 bytes for 	}
                                        { 16-bit, 8 bytes for 32-bit int's) 				}
      v_n{base}: ush;						 { literal, length base, or distance base 		}
      v_t: phuftlist;                   { Linked List 											}
   End;
   HuftList = Array[0..SizeOf(Huft) * 1024{8190}] Of huft;

Var
   WSIZE: Integer;
   Slide: PChar;                        { sliding dictionary for unzipping					}
   Inbuf: Iobuf;                        { input buffer											}
   Inpos,
   	ReadPos: Integer;                 { position in input buffer, position read from file}
   w: Integer; //Word; Def64            { current position in slide							}
   b: Longint;                          { bit buffer												}
   k: Byte;                             { bits in bit buffer									}
   ReachedSize: Int64;                  { number of bytes read from zipfile				}
   zipEOF: Boolean;                     { read over end of zip section for this file	}

//-------------------------------------------------------------

Procedure huft_free(t: phuftlist);
Var
   p, q: phuftlist;
   z: Integer;
Begin
   p := Pointer(t);
   While p <> Nil Do
   Begin
      dec(Longint(p), SizeOf(huft));
      q := p^[0].v_t;
      z := p^[0].v_n;                   { size in bytes, required by TP }
      FreeMem(p, (z + 1) * SizeOf(huft));
      p := q
   End;
End;
//-------------------------------------------------------------

Procedure DumpBits(n: Byte);
Begin
   b := b Shr n;
   k := k - n;
End;
//-------------------------------------------------------------

//Flush w bytes directly from slide to file
Function Flush(w: Word32): Boolean;
Var
   n: Integer;
   ProgressByFile,
      ProgressByArchive: Byte;
Begin
   dec(InflateProc.pProgressPos^, w);
   dec(Bytes_To_Go, w);

   n := WriteProc(Outfile, Slide[0], False, 32, w, dtData);
   Result := (Word32(n) = w);

   Try
      ProgressByFile :=
         ztvGbls.CalcProgress64(
         InflateRec.UnpackedSize - Bytes_To_Go,
         InflateRec.UnpackedSize);

      ProgressByArchive :=
         ztvGbls.CalcProgress64(
         InflateRec.TotalArchiveSize - pProgressPos^,
         InflateRec.TotalArchiveSize);

      ProgressProc(ProgressByFile, ProgressByArchive);
   Except
      //On e: exception Do ShowMessage( e.message )
   End;
End;
//-------------------------------------------------------------

Procedure ReadBuf;
Begin
   If ReachedSize > InflateRec.PackedSize + 2 Then
   Begin                                {+2: last code is smaller than requested!}
      ReadPos := SizeOf(Inbuf);         {Simulates reading -> no blocking}
      zipEOF := True
   End
   Else
   Begin
      If InflateRec.PackedSize - ReachedSize < INBUFSIZ Then
         ReadPos := ReadProc(Infile, Nil, Inbuf, (InflateRec.BitFlag And 1) = 1,
            0, InflateRec.PackedSize - ReachedSize, dtData)
      Else
         ReadPos := ReadProc(Infile, Nil, Inbuf, (InflateRec.BitFlag And 1) = 1,
            0, INBUFSIZ, dtData);

      If (ReadPos = 0) Then
      Begin                             {readpos=0: kein Fehler gemeldet!!!}
         ReadPos := SizeOf(Inbuf);      {Simulates reading -> CRC error}
         zipEOF := True;
      End;

      Inc(ReachedSize, ReadPos);
      dec(ReadPos);                     {Reason: index of inbuf starts at 0}
   End;
   Inpos := 0;
End;
//-------------------------------------------------------------

Procedure NeedBits(n: Byte);
Var
   nb: Longint;
Begin
   While k < n Do
   Begin
      If Inpos > ReadPos Then
         ReadBuf();
      nb := Inbuf[Inpos];
      Inc(Inpos);
      b := b Or nb Shl k;
      Inc(k, 8);
   End;
End;
//-------------------------------------------------------------

Function huft_build(b: pWord; n: Word; s: Word; d, e: PushList; t: pphuft; Var m:
   Integer): Integer;
Var
   a: Word;                             { counter for codes of length k }
   c: Array[0..b_max + 1] Of Word;      { bit length count table }
   f: Word;                             { i repeats in table every f entries }
   g,                                   { max. code length }
   h: Integer;                          { table level }
   i: Word;                             { counter, current code }
   j: Word;                             { counter }
   k: Integer;                          { number of bits in current code }
   p: pWord;                            { pointer into c, b and v }
   q: phuftlist;                        { points to current table }
   r: huft;                             { table entry for structure assignment }
   u: Array[0..b_max] Of phuftlist;     { table stack }
   v: Array[0..n_max] Of Word;          { values in order of bit length }
   w: Integer;                          { bits before this table }
   x: Array[0..b_max + 1] Of Word;      { bit offsets, then code stack }
   l: Array[-1..b_max + 1] Of Word;     { l[h] bits in table of level h }
   xp: pWord;                           { pointer into x }
   y: Integer;                          { number of dummy codes added }
   z: Word;                             { number of entries in current table }
   TryAgain: Boolean;                   { boolean for loop }
   pt: phuft;                           { for test against bad input }
   el: Word;                            { length of eob code=code 256 }
Begin
   If n > 256 Then
      el := pWord(Longint(b) + 256 * SizeOf(Word))^
   Else
      el := BMAX;

   {generate counts for each bit length}
   FillChar(c, SizeOf(c), #0);
   p := b;
   i := n;                              {p points to array of word}

   Repeat
      If p^ > b_max Then
      Begin
         t^ := Nil;
         m := 0;
         Result := huft_error;
         exit
      End;
      Inc(c[p^]);
      Inc(Longint(p), SizeOf(Word));    {point to next item}
      dec(i);
   Until i = 0;

   If c[0] = n Then
   Begin
      t^ := Nil;
      m := 0;
      Result := huft_complete;
      exit
   End;

   {find minimum and maximum length, bound m by those}
   j := 1;
   While (j <= b_max) And (c[j] = 0) Do
      Inc(j);

   k := j;

   If m < j Then
      m := j;

   i := b_max;
   While (i > 0) And (c[i] = 0) Do
      dec(i);

   g := i;

   If m > i Then
      m := i;

   {adjust last length count to fill out codes, if needed}
   y := 1 Shl j;
   While j < i Do
   Begin
      y := y - c[j];
      If y < 0 Then
      Begin
         Result := huft_error;
         exit
      End;
      y := y Shl 1;
      Inc(j);
   End;

   dec(y, c[i]);
   If y < 0 Then
   Begin
      Result := huft_error;
      exit
   End;

   Inc(c[i], y);

   {generate starting offsets into the value table for each length}
   x[1] := 0;
   j := 0;
   p := pWord(@c);
   Inc(Longint(p), SizeOf(Word));
   xp := pWord(@x);
   Inc(Longint(xp), 2 * SizeOf(Word));
   dec(i);

   While i <> 0 Do
   Begin
      Inc(j, p^);
      xp^ := j;
      Inc(Longint(p), 2);
      Inc(Longint(xp), 2);
      dec(i);
   End;

   {make table of values in order of bit length}
   p := b;
   i := 0;
   Repeat
      j := p^;
      Inc(Longint(p), SizeOf(Word));
      If j <> 0 Then
      Begin
         v[x[j]] := i;
         Inc(x[j]);
      End;
      Inc(i);
   Until i >= n;

   {generate huffman codes and for each, make the table entries}
   x[0] := 0; i := 0;
   p := pWord(@v);
   h := -1;
   l[-1] := 0;
   w := 0;
   u[0] := Nil;
   q := Nil;
   z := 0;

   {go through the bit lengths (k already is bits in shortest code)}
   For k := k To g Do
   Begin
      For a := c[k] Downto 1 Do
      Begin
         {here i is the huffman code of length k bits for value p^}
         While k > w + l[h] Do
         Begin
            Inc(w, l[h]);               {length of tables to this position}
            Inc(h);
            z := g - w;

            If z > m Then
               z := m;

            j := k - w;
            f := 1 Shl j;

            If f > a + 1 Then
            Begin
               dec(f, a + 1);
               xp := @c[k];
               Inc(j);
               TryAgain := True;
               While (j < z) And TryAgain Do
               Begin
                  f := f Shl 1;
                  Inc(Longint(xp), SizeOf(Word));
                  If f <= xp^ Then
                     TryAgain := False
                  Else
                  Begin
                     dec(f, xp^);
                     Inc(j);
                  End;
               End;
            End;

            If (w + j > el) And (w < el) Then
               j := el - w;             {make eob code end at table}

            If w = 0 Then
            Begin
               j := m;                  {fix: main table always m bits}
            End;

            z := 1 Shl j;
            l[h] := j;

            {allocate and link new table}
            GetMem(q, (z + 1) * SizeOf(huft));
            If q = Nil Then
            Begin
               If h <> 0 Then
                  huft_free(Pointer(u[0]));
               Result := huft_outofmem;
               exit
            End;

            FillChar(q^, (z + 1) * SizeOf(huft), #0);
            q^[0].v_n := z;             {size of table, needed in freemem ***}

            t^ := @q^[1];               {first item starts at 1}
            t := pphuft(@q^[0].v_t);
            t^ := Nil;
            q := phuftlist(@q^[1]); {pointer(longint(q)+sizeof(huft));} {???}
            u[h] := q;

            {connect to last table, if there is one}
            If h <> 0 Then
            Begin
               x[h] := i;
               r.b := l[h - 1];
               r.e := AdditionalBitsInTable + j;
               r.v_t := q;
               j := (i And ((1 Shl w) - 1)) Shr (w - l[h - 1]);

               {test against bad input!}
               pt := phuft(Longint(u[h - 1]) - SizeOf(huft));
               If j > pt^.v_n Then
               Begin
                  huft_free(Pointer(u[0]));
                  Result := huft_error;
                  exit
               End;

               pt := @u[h - 1]^[j];
               pt^ := r;
            End;
         End;

         {set up table entry in r}
         r.b := Word(k - w);
         r.v_t := Nil; 						 {unused}

         If Longint(p) >= Longint(@v[n]) Then
            r.e := InvalidCode
         Else
            If p^ < s Then
            Begin
            	If AdditionalBitsInTable = 16 Then
               Begin
                  If p^ < 256 Then
                     r.e := 16
                  Else
                     r.e := 15;
               End Else
                  If p^ < 256 Then
                     r.e := ValidCode
                  Else
                     r.e := EndOfBlock;

               r.v_n := p^;
               Inc(Longint(p), SizeOf(Word));
            End
            Else
            Begin
               If (d = Nil) Or (e = Nil) Then
               Begin
                  huft_free(Pointer(u[0]));
                  Result := huft_error;
                  exit
               End;
               r.e := Word(e^[p^ - s] + NonSimpleLookup);
               r.e := e^[p^ - s];
               r.v_n := d^[p^ - s];
               Inc(Longint(p), SizeOf(Word));
            End;

         {fill code like entries with r}
         f := 1 Shl (k - w);
         j := i Shr w;
         While j < z Do
         Begin
            q^[j] := r;
            Inc(j, f);
         End;

         {backwards increment the k-bit code i}
         j := 1 Shl (k - 1);
         While (i And j) <> 0 Do
         Begin
            {i:=i^j;}
            i := i Xor j;
            j := j Shr 1;
         End;
         i := i Xor j;

         {backup over finished tables}
         While ((i And ((1 Shl w) - 1)) <> x[h]) Do
         Begin
            dec(h);
            dec(w, l[h]);               {size of previous table!}
         End;
      End;
   End;

   m := Integer(l[0]);

   If (y <> 0) And (g <> 1) Then
      Result := huft_incomplete
   Else
      Result := huft_complete;
End;
//-------------------------------------------------------------

(* "Decompress" an inflated type 0 ( stored ) block. *)

Function inflate_stored: Integer;
Var
   n: Word;                             {number of bytes in block}
Begin
   {go to byte boundary}
   n := k And 7;
   DumpBits(n);
   {get the length and its complement}
   NeedBits(16);
   n := b And $FFFF;
   DumpBits(16);
   NeedBits(16);
   If (n <> (Not b) And $FFFF) Then
   Begin
      inflate_stored := unzip_ZipFileErr;
      exit
   End;
   DumpBits(16);
   While (n > 0) And Not (pCancel^ Or zipEOF) Do
   Begin                                {read and output the compressed data}
      dec(n);
      NeedBits(8);
      Slide[w] := Char(b);
      Inc(w);
      If w = WSIZE Then
      Begin
         If Not Flush(w) Then
         Begin
            Result := unzip_WriteErr;
            exit
         End;
         w := 0;
      End;
      DumpBits(8);
   End;

   If pCancel^ Then
      Result := unzip_UserAbort
   Else
      If zipEOF Then
         Result := unzip_ReadErr
      Else
         Result := unzip_Ok;
End;
//-------------------------------------------------------------

Function inflate_codes(tl, td: phuftlist; bl, bd: Integer): Integer;
Var
   n, d: Cardinal;
   e1,                                  //length and index for copy
   ml, md: Word;                        //masks for bl and bd bits
   t: phuft;                            //pointer to table entry
   e: Byte;                             //table entry flag/number of extra bits
Begin

   // inflate the coded data
   ml := mask_bits[bl];                 //precompute masks for speed
   md := mask_bits[bd];

   While Not (pCancel^ Or zipEOF) Do
   Begin
      NeedBits(bl);
      t := @tl^[b And ml];
      e := t^.e;

      If e > ValidCode Then
         Repeat                         // it's a literal
            If e = InvalidCode Then
            Begin
               Result := unzip_ZipFileErr;
               exit
            End;
            DumpBits(t^.b);
            dec(e, ValidCode);
            NeedBits(e);
            t := @t^.v_t^[b And mask_bits[e]];
            e := t^.e;
         Until e <= ValidCode;

      DumpBits(t^.b);
      If e = ValidCode Then
      Begin
         Slide[w] := Char(t^.v_n);
         Inc(w);
         If w = WSIZE Then
         Begin
            If Not Flush(w) Then
            Begin
               Result := unzip_WriteErr;
               exit;
            End;
            w := 0
         End;
      End
      Else
      Begin                             //it's an EOB or a length
         If e = EndOfBlock Then
         Begin                          //exit if end of block
            Result := unzip_Ok;
            exit;
         End;

         NeedBits(e);                   //get length of block to copy
         n := t^.v_n + (b And mask_bits[e]);
         DumpBits(e);
         NeedBits(bd);                  //Decode distance of block to copy
         t := @td^[b And md];

         e := t^.e;
         If e > ValidCode Then
            Repeat
               If e = InvalidCode Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);

               dec(e, ValidCode);
               NeedBits(e);
               t := @t^.v_t^[b And mask_bits[e]];
               e := t^.e;
            Until e <= ValidCode;

         DumpBits(t^.b);
         NeedBits(e);
         d := w - t^.v_n - b And mask_bits[e];
         DumpBits(e);

         //do the copy
         Repeat
            If pCancel^ Then
               Break;
            d := d And (WSIZE - 1);
            If d > w Then
               e1 := WSIZE - d
            Else
               e1 := WSIZE - w;

            If e1 > n Then
               e1 := n;

            dec(n, e1);

            // v6.4.2 revised ==================================
            // changed for speed.  The "move" procedure does not
            // work due to overlap issues.  CopyMem works great
            // and much faster!
            // =================================================
            If (w - d >= e1) Then
            Begin
               Move(Slide[d], Slide[w], e1);
               Inc(w, e1);
               Inc(d, e1);
            End
            Else
            Begin
               Repeat
                  Slide[w] := Slide[d];
                  Inc(w);
                  Inc(d);
                  dec(e1);
               Until (e1 = 0);
            End;
            //CopyMem(@Slide[d], @Slide[w], e1);
            //Inc(w, e1);
            //Inc(d, e1);
            // v6.4.2 ==========================================

            If w = WSIZE Then
            Begin
               If (Not pCancel^) And (Not Flush(w)) Then
               Begin
                  Result := unzip_WriteErr;
                  exit;
               End;
               w := 0;
            End;

         Until n = 0;
      End;
   End;

   If pCancel^ Then
      inflate_codes := unzip_UserAbort
   Else
      inflate_codes := unzip_ReadErr;
End;
//-------------------------------------------------------------

Function inflate_fixed: Integer;
Var
   i: Integer;                          { temporary variable }
   tl,                                  { literal/length code table }
   td: phuftlist;                       { distance code table }
   bl, bd: Integer;                     { lookup bits for tl/bd }
   l: Array[0..287] Of Word;            { length list for huft_build }
Begin
   {set up literal table}
   For i := 0 To 143 Do
      l[i] := 8;
   For i := 144 To 255 Do
      l[i] := 9;
   For i := 256 To 279 Do
      l[i] := 7;
   For i := 280 To 287 Do
      l[i] := 8;                        {make a complete, but wrong code set}

   bl := 7;
   If Is64Bit Then
      i := huft_build(pWord(@l), 288, 257, PushList(@cplens64), PushList(@cplext64),
         pphuft(@tl), bl)
   Else
      i := huft_build(pWord(@l), 288, 257, PushList(@cplens), PushList(@cplext),
         pphuft(@tl), bl);

   If i <> huft_complete Then
   Begin
      Result := i;
      exit
   End;

   For i := 0 To 29 Do
      l[i] := 5;                        {make an incomplete code set}

   bd := 5;
   If Is64Bit Then
      i := huft_build(pWord(@l), 30, 0, PushList(@cpdist64), PushList(@cpdext64),
         pphuft(@td), bd)
   Else
      i := huft_build(pWord(@l), 30, 0, PushList(@cpdist), PushList(@cpdext),
         pphuft(@td), bd);

   If i > huft_incomplete Then
   Begin
      huft_free(tl);
      Result := unzip_ZipFileErr;
      exit
   End;

   Result := inflate_codes(tl, td, bl, bd);
   huft_free(tl);
   huft_free(td);
End;
//-------------------------------------------------------------

(* Decompress an inflated type 2 ( dynamic Huffman codes ) block. *)

Function inflate_dynamic: Integer;
Var
   i: Integer;                          { temporary variables }
   j,
      l,                                { last length }
   m,                                   { mask for bit length table }
   n: Word;                             { number of lengths to get }
   tl,                                  { literal/length code table }
   td: phuftlist;                       { distance code table }
   bl, bd: Integer;                     { lookup bits for tl/bd }
   nb, nl, nd: Word; 						 { number of bit length/literal length/distance codes }
   ll: Array[0..288 + 32 - 1] Of Word;  { literal/length and distance code lengths }

Begin
   //read in table lengths
   NeedBits(5);
   nl := 257 + Word(b) And $1F;
   DumpBits(5);
   NeedBits(5);
   nd := 1 + Word(b) And $1F;
   DumpBits(5);
   NeedBits(4);
   nb := 4 + Word(b) And $F;
   DumpBits(4);

   If (nl > 288) Or (nd > 32) Then
   Begin
      Result := 1;
      exit
   End;

   FillChar(ll, SizeOf(ll), #0);

   {read in bit-length-code lengths}
   For j := 0 To nb - 1 Do
   Begin
      NeedBits(3);
      ll[Border[j]] := b And 7;
      DumpBits(3);
   End;

   For j := nb To 18 Do
      ll[Border[j]] := 0;

   {build Decoding table for trees--single level, 7 bit lookup}
   bl := 7;
   If Is64Bit Then
      i := huft_build(pWord(@ll), 19, 19, Nil, Nil, pphuft(@tl), bl)
   Else
      i := huft_build(pWord(@ll), 19, 19, Nil, Nil, pphuft(@tl), bl);

   If i <> huft_complete Then
   Begin
      If i = huft_incomplete Then
         huft_free(tl);                 {other errors: already freed}
      Result := unzip_ZipFileErr;
      exit;
   End;

   {read in literal and distance code lengths}
   n := nl + nd;
   m := mask_bits[bl];
   l := 0;

   i := 0;
   While Word(i) < n Do
   Begin
      NeedBits(bl);
      td := phuftlist(@tl^[b And m]);
      j := phuft(td)^.b;
      DumpBits(j);
      j := phuft(td)^.v_n;
      If j < 16 Then
      Begin                             {length of code in bits (0..15)}
         l := j;                        {save last length in l}
         ll[i] := l;
         Inc(i)
      End
      Else
         If j = 16 Then
         Begin                          {repeat last length 3 to 6 times}
            NeedBits(2);
            j := 3 + b And 3;
            DumpBits(2);

            If i + j > n Then
            Begin
               inflate_dynamic := 1;
               exit
            End;

            While j > 0 Do
            Begin
               ll[i] := l;
               dec(j);
               Inc(i);
            End;
         End
         Else
            If j = 17 Then
            Begin                       {3 to 10 zero length codes}
               NeedBits(3);
               j := 3 + b And 7;
               DumpBits(3);

               If i + j > n Then
               Begin
                  inflate_dynamic := 1;
                  exit
               End;

               While j > 0 Do
               Begin
                  ll[i] := 0;
                  Inc(i);
                  dec(j);
               End;
               l := 0;
            End
            Else
            Begin                       {j == 18: 11 to 138 zero length codes}
               NeedBits(7);
               j := 11 + b And $7F;
               DumpBits(7);

               If i + j > n Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;

               While j > 0 Do
               Begin
                  ll[i] := 0;
                  dec(j);
                  Inc(i);
               End;
               l := 0;
            End;
   End;

   huft_free(tl);                       {free Decoding table for trees}

   {build the Decoding tables for literal/length and distance codes}
   bl := lbits;
   If Is64Bit Then
      i := huft_build(pWord(@ll), nl, 257, PushList(@cplens64), PushList(@cplext64),
         pphuft(@tl), bl)
   Else
      i := huft_build(pWord(@ll), nl, 257, PushList(@cplens), PushList(@cplext),
         pphuft(@tl), bl);

   If i <> huft_complete Then
   Begin
      If i = huft_incomplete Then
         huft_free(tl);
      Result := unzip_ZipFileErr;
      exit
   End;

   bd := dbits;
   If Is64Bit Then
      i := huft_build(pWord(@ll[nl]), nd, 0, PushList(@cpdist64), PushList(@cpdext64),
         pphuft(@td), bd)
   Else
      i := huft_build(pWord(@ll[nl]), nd, 0, PushList(@cpdist), PushList(@cpdext),
         pphuft(@td), bd);

   If i > huft_incomplete Then
   Begin                                {pkzip bug workaround}
      If i = huft_incomplete Then
         huft_free(td);
      huft_free(tl);
      Result := unzip_ZipFileErr;
      exit
   End;

   {Decompress until an end-of-block code}
   Result := inflate_codes(tl, td, bl, bd);
   huft_free(tl);
   huft_free(td);
End;
//-------------------------------------------------------------

Function inflate_block(Var e: Integer): Integer;
Var
   t: Word;                             {block type}

Begin
   NeedBits(1);
   e := b And 1;
   DumpBits(1);

   NeedBits(2);
   t := b And 3;
   DumpBits(2);

   Case t Of
      0: Result := inflate_stored;
      1: Result := inflate_fixed;
      2: Result := inflate_dynamic;
   Else
      Result := unzip_ZipFileErr;       {bad block type}
   End;
End;
//-------------------------------------------------------------

(* The file position is set prior to entering this unit. *)

Function Inflate(Inf: TStream32; Var Outf: TStream32; IR: TInflateRec;
   ABIT: Byte; InflateProc: TInflateProc): Boolean;

   Function _Inflate: Integer;
   Var
      e,                                {last block flag}
      r: Integer;                       {result code}
   Begin
      Inpos := 0;                       {Input buffer position}
      ReadPos := -1;                    {Nothing read}
      ReachedSize := 0;
      zipEOF := False;

      {initialize window, bit buffer}
      w := 0;
      k := 0;
      b := 0;

      {Decompress until the last block}
      Repeat
         r := inflate_block(e);
         If pCancel^ Then
            Break;
         If r <> 0 Then
         Begin
            Result := r;
            exit
         End;
      Until e <> 0;

      {flush out slide}
      If Not Flush(w) Then
         Result := unzip_WriteErr
      Else
         Result := unzip_Ok;
   End;

Begin
   //WindowPtr^.zWindow := @Inbuf[0];
   //WindowPtr^.zEnd := Pointer(Ptr2Int(WindowPtr^.zWindow) + SizeOf(Inbuf));
   //Result := True;
   Infile := Inf;
   Outfile := Outf;
   InflateRec := IR;
	AdditionalBitsInTable := ABIT;

   ReadProc := InflateProc.RB;
   WriteProc := InflateProc.WB;
   ProgressProc := InflateProc.PP;
   pProgressPos := InflateProc.pProgressPos;
   pCancel := InflateProc.pCancel;
   Bytes_To_Go := InflateRec.UnpackedSize;

   If Is64Bit Then
      WSIZE := 65536
   Else
      WSIZE := 32768;

   GetMem(Slide, WSIZE);
   Try
      Result := _Inflate() = unzip_Ok;
   Finally
      FreeMem(Slide, WSIZE);
   End;
End;

//-------------------------------------------------------------
//-------------------------------------------------------------
//
// Explode()  ...start
//
//-------------------------------------------------------------
//-------------------------------------------------------------
Const
   cplen2: Array[0..63] Of word16 =
      (2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
      18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
      34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49,
      50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65);

   cplen3: Array[0..63] Of word16 =
      (3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
      19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34,
      35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
      51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66);

   extra: Array[0..63] Of word16 =
      (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 8);

   cpdist4: Array[0..63] Of word16 =
      (1, 65, 129, 193, 257, 321, 385, 449, 513, 577, 641,
      705, 769, 833, 897, 961, 1025, 1089, 1153, 1217, 1281, 1345,
      1409, 1473, 1537, 1601, 1665, 1729, 1793, 1857, 1921, 1985, 2049,
      2113, 2177, 2241, 2305, 2369, 2433, 2497, 2561, 2625, 2689, 2753,
      2817, 2881, 2945, 3009, 3073, 3137, 3201, 3265, 3329, 3393, 3457,
      3521, 3585, 3649, 3713, 3777, 3841, 3905, 3969, 4033);

   cpdist8: Array[0..63] Of word16 =
      (1, 129, 257, 385, 513, 641, 769, 897, 1025, 1153, 1281,
      1409, 1537, 1665, 1793, 1921, 2049, 2177, 2305, 2433, 2561, 2689,
      2817, 2945, 3073, 3201, 3329, 3457, 3585, 3713, 3841, 3969, 4097,
      4225, 4353, 4481, 4609, 4737, 4865, 4993, 5121, 5249, 5377, 5505,
      5633, 5761, 5889, 6017, 6145, 6273, 6401, 6529, 6657, 6785, 6913,
      7041, 7169, 7297, 7425, 7553, 7681, 7809, 7937, 8065);

//-------------------------------------------------------------

Procedure ReadByte(Var bt: Byte);
Begin
   If Inpos > ReadPos Then
      ReadBuf();
   bt := Inbuf[Inpos];
   Inc(Inpos);
End;
//-------------------------------------------------------------

Function GetTree(l: pWord; n: Word): Integer;
Var
   i, k, j, b: Word;
   bytebuf: Byte;

Begin
   ReadByte(bytebuf);
   i := bytebuf;
   Inc(i);
   k := 0;
   Repeat
      ReadByte(bytebuf);
      j := bytebuf;
      b := (j And $F) + 1;
      j := ((j And $F0) Shr 4) + 1;
      If (k + j) > n Then
      Begin
         Result := 4;
         exit
      End;

      Repeat
         l^ := b;
         Inc(Longint(l), SizeOf(word16));
         Inc(k);
         dec(j);
      Until j = 0;
      dec(i);
   Until i = 0;
   
   If k <> n Then
      Result := 4
   Else
      Result := 0;
End;
//-------------------------------------------------------------

Function explode_lit8(tb, tl, td: phuftlist; bb, bl, bd: Integer): Integer;
Var
   s: Longint;
   e: Word;
   n, d: Word;
   w: Word;
   t: phuft;
   mb, ml, md: Word;
   u: Word;

Begin
   b := 0;
   k := 0;
   w := 0;

   u := 1;
   mb := mask_bits[bb];
   ml := mask_bits[bl];
   md := mask_bits[bd];
   s := InflateRec.UnpackedSize;
   While (s > 0) And Not (pCancel^ Or zipEOF) Do
   Begin
      NeedBits(1);
      If (b And 1) <> 0 Then
      Begin                             {Literal}
         DumpBits(1);
         dec(s);
         NeedBits(bb);
         t := @tb^[(Not b) And mb];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;

         DumpBits(t^.b);
         Slide[w] := Char(t^.v_n);
         Inc(w);
         If w = WSIZE Then
         Begin
            If Not Flush(w) Then
            Begin
               Result := unzip_WriteErr;
               exit
            End;
            w := 0; u := 0;
         End;
      End
      Else
      Begin
         DumpBits(1);
         NeedBits(7);
         d := b And $7F;
         DumpBits(7);
         NeedBits(bd);
         t := @td^[(Not b) And md];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;
         DumpBits(t^.b);

         d := w - d - t^.v_n;
         NeedBits(bl);
         t := @tl^[(Not b) And ml];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;

         DumpBits(t^.b);

         n := t^.v_n;
         If e <> 0 Then
         Begin
            NeedBits(8);
            Inc(n, Byte(b) And $FF);
            DumpBits(8);
         End;
         dec(s, n);
         Repeat
            d := d And pred(WSIZE);
            If d > w Then
               e := WSIZE - d
            Else
               e := WSIZE - w;
            If e > n Then
               e := n;
            dec(n, e);
            If (u <> 0) And (w <= d) Then
            Begin
               FillChar(Slide[w], e, #0);
               Inc(w, e);
               Inc(d, e);
            End
            Else
               If (w - d >= e) Then
               Begin
                  Move(Slide[d], Slide[w], e);
                  Inc(w, e);
                  Inc(d, e);
               End
               Else
                  Repeat
                     Slide[w] := Slide[d];
                     Inc(w);
                     Inc(d);
                     dec(e);
                  Until e = 0;
            If w = WSIZE Then
            Begin
               If Not Flush(w) Then
               Begin
                  Result := unzip_WriteErr;
                  exit
               End;
               w := 0; u := 0;
            End;
         Until n = 0;
      End;
   End;

   If pCancel^ Then
      Result := unzip_UserAbort
   Else
      If Not Flush(w) Then
         Result := unzip_WriteErr
      Else
         If zipEOF Then
            Result := unzip_ReadErr
         Else
            Result := unzip_Ok;
End;
//-------------------------------------------------------------

Function explode_lit4(tb, tl, td: phuftlist; bb, bl, bd: Integer): Integer;
Var
   s: Longint;
   e: Word;
   n, d: Word;
   w: Word;
   t: phuft;
   mb, ml, md: Word;
   u: Word;
Begin
   b := 0; k := 0; w := 0;
   u := 1;
   mb := mask_bits[bb];
   ml := mask_bits[bl];
   md := mask_bits[bd];
   s := InflateRec.UnpackedSize;
   While (s > 0) And Not (pCancel^ Or zipEOF) Do
   Begin
      NeedBits(1);
      If (b And 1) <> 0 Then
      Begin                             {Literal}
         DumpBits(1);
         dec(s);
         NeedBits(bb);
         t := @tb^[(Not b) And mb];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;

         DumpBits(t^.b);
         Slide[w] := Char(t^.v_n);
         Inc(w);
         If w = WSIZE Then
         Begin
            If Not Flush(w) Then
            Begin
               Result := unzip_WriteErr;
               exit
            End;
            w := 0; u := 0;
         End;
      End
      Else
      Begin
         DumpBits(1);
         NeedBits(6);
         d := b And $3F;
         DumpBits(6);
         NeedBits(bd);
         t := @td^[(Not b) And md];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;
         DumpBits(t^.b);
         d := w - d - t^.v_n;
         NeedBits(bl);
         t := @tl^[(Not b) And ml];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;

         DumpBits(t^.b);
         n := t^.v_n;
         If e <> 0 Then
         Begin
            NeedBits(8);
            Inc(n, b And $FF);
            DumpBits(8);
         End;
         dec(s, n);
         Repeat
            d := d And Pred(WSIZE);

            If d > w Then
               e := WSIZE - d
            Else
               e := WSIZE - w;

            If e > n Then
               e := n;
            dec(n, e);

            If (u <> 0) And (w <= d) Then
            Begin
               FillChar(Slide[w], e, #0);
               Inc(w, e);
               Inc(d, e);
            End
            Else
               If (w - d >= e) Then
               Begin
                  Move(Slide[d], Slide[w], e);
                  Inc(w, e);
                  Inc(d, e);
               End
               Else
                  Repeat
                     Slide[w] := Slide[d];
                     Inc(w);
                     Inc(d);
                     dec(e);
                  Until e = 0;

            If w = WSIZE Then
            Begin
               If Not Flush(w) Then
               Begin
                  Result := unzip_WriteErr;
                  exit
               End;
               w := 0; u := 0;
            End;
         Until n = 0;
      End;
   End;

   If pCancel^ Then
      Result := unzip_UserAbort
   Else
      If Not Flush(w) Then
         Result := unzip_WriteErr
      Else
         If zipEOF Then
            Result := unzip_ReadErr
         Else
            Result := unzip_Ok;
End;
//-------------------------------------------------------------

Function explode_nolit8(tl, td: phuftlist; bl, bd: Integer): Integer;
Var
   s: Longint;
   e: Word;
   n, d: Word;
   w: Word;
   t: phuft;
   ml, md: Word;
   u: Word;

Begin
   b := 0; k := 0; w := 0;
   u := 1;
   ml := mask_bits[bl];
   md := mask_bits[bd];
   s := InflateRec.UnpackedSize;
   While (s > 0) And Not (pCancel^ Or zipEOF) Do
   Begin
      NeedBits(1);
      If (b And 1) <> 0 Then
      Begin                             {Literal}
         DumpBits(1);
         dec(s);
         NeedBits(8);
         Slide[w] := Char(b);
         Inc(w);
         If w = WSIZE Then
         Begin
            If Not Flush(w) Then
            Begin
               Result := unzip_WriteErr;
               exit
            End;
            w := 0; u := 0;
         End;
         DumpBits(8);
      End
      Else
      Begin
         DumpBits(1);
         NeedBits(7);
         d := b And $7F;
         DumpBits(7);
         NeedBits(bd);
         t := @td^[(Not b) And md];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;
         DumpBits(t^.b);

         d := w - d - t^.v_n;
         NeedBits(bl);
         t := @tl^[(Not b) And ml];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;

         DumpBits(t^.b);

         n := t^.v_n;
         If e <> 0 Then
         Begin
            NeedBits(8);
            Inc(n, b And $FF);
            DumpBits(8);
         End;
         dec(s, n);
         Repeat
            d := d And pred(WSIZE);
            If d > w Then
               e := WSIZE - d
            Else
               e := WSIZE - w;
            If e > n Then
               e := n;
            dec(n, e);
            If (u <> 0) And (w <= d) Then
            Begin
               FillChar(Slide[w], e, #0);
               Inc(w, e);
               Inc(d, e);
            End
            Else
               If (w - d >= e) Then
               Begin
                  Move(Slide[d], Slide[w], e);
                  Inc(w, e);
                  Inc(d, e);
               End
               Else
                  Repeat
                     Slide[w] := Slide[d];
                     Inc(w);
                     Inc(d);
                     dec(e);
                  Until e = 0;

            If w = WSIZE Then
            Begin
               If Not Flush(w) Then
               Begin
                  Result := unzip_WriteErr;
                  exit
               End;
               w := 0; u := 0;
            End;
         Until n = 0;
      End;
   End;
   
   If pCancel^ Then
      Result := unzip_UserAbort
   Else
      If Not Flush(w) Then
         Result := unzip_WriteErr
      Else
         If zipEOF Then
            Result := unzip_ReadErr
         Else
            Result := unzip_Ok;
End;
//-------------------------------------------------------------

Function explode_nolit4(tl, td: phuftlist; bl, bd: Integer): Integer;
Var
   s: Longint;
   e: Word;
   n, d: Word;
   w: Word;
   t: phuft;
   ml, md: Word;
   u: Word;

Begin
   b := 0; k := 0; w := 0;
   u := 1;
   ml := mask_bits[bl];
   md := mask_bits[bd];
   s := InflateRec.UnpackedSize;
   While (s > 0) And Not (pCancel^ Or zipEOF) Do
   Begin
      NeedBits(1);
      If (b And 1) <> 0 Then
      Begin                             {Literal}
         DumpBits(1);
         dec(s);
         NeedBits(8);
         Slide[w] := Char(b);
         Inc(w);

         If w = WSIZE Then
         Begin
            If Not Flush(w) Then
            Begin
               Result := unzip_WriteErr;
               exit
            End;
            w := 0; u := 0;
         End;
         DumpBits(8);
      End
      Else
      Begin
         DumpBits(1);
         NeedBits(6);
         d := b And $3F;
         DumpBits(6);
         NeedBits(bd);
         t := @td^[(Not b) And md];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;
         DumpBits(t^.b);
         d := w - d - t^.v_n;
         NeedBits(bl);
         t := @tl^[(Not b) And ml];
         e := t^.e;
         If e > 16 Then
            Repeat
               If e = 99 Then
               Begin
                  Result := unzip_ZipFileErr;
                  exit
               End;
               DumpBits(t^.b);
               dec(e, 16);
               NeedBits(e);
               t := @t^.v_t^[(Not b) And mask_bits[e]];
               e := t^.e;
            Until e <= 16;

         DumpBits(t^.b);
         n := t^.v_n;
         If e <> 0 Then
         Begin
            NeedBits(8);
            Inc(n, b And $FF);
            DumpBits(8);
         End;
         dec(s, n);
         Repeat
            d := d And pred(WSIZE);
            If d > w Then
               e := WSIZE - d
            Else
               e := WSIZE - w;
            If e > n Then
               e := n;
            dec(n, e);
            If (u <> 0) And (w <= d) Then
            Begin
               FillChar(Slide[w], e, #0);
               Inc(w, e);
               Inc(d, e);
            End
            Else
               If (w - d >= e) Then
               Begin
                  Move(Slide[d], Slide[w], e);
                  Inc(w, e);
                  Inc(d, e);
               End
               Else
                  Repeat
                     Slide[w] := Slide[d];
                     Inc(w);
                     Inc(d);
                     dec(e);
                  Until e = 0;

            If w = WSIZE Then
            Begin
               If Not Flush(w) Then
               Begin
                  Result := unzip_WriteErr;
                  exit
               End;
               w := 0; u := 0;
            End;
         Until n = 0;
      End;
   End;

   If pCancel^ Then
      Result := unzip_UserAbort
   Else
      If Not Flush(w) Then
         Result := unzip_WriteErr
      Else
         If zipEOF Then
            Result := unzip_ReadErr
         Else
            Result := unzip_Ok;
End;
//-------------------------------------------------------------

Function Explode(Inf: TStream32; Var Outf: TStream32;
   IR: TInflateRec; ABIT: Byte; InflateProc: TInflateProc): Boolean;

   Function _Explode: Integer;
   Var
      r: Integer;
      tb, tl, td: phuftlist;
      bb, bl, bd: Integer;
      l: Array[0..255] Of Word;
   Begin
      Inpos := 0;
      ReadPos := -1;                    {Nothing read in}
      ReachedSize := 0;
      zipEOF := False;

      bl := 7;
      If InflateRec.PackedSize > 200000 Then
         bd := 8
      Else
         bd := 7;

      If hufttype And 4 <> 0 Then
      Begin
         bb := 9;

         r := GetTree(@l[0], 256);
         If r <> 0 Then
         Begin
            Result := unzip_ZipFileErr;
            exit
         End;

         r := huft_build(pWord(@l), 256, 256, Nil, Nil, pphuft(@tb), bb);
         If r <> 0 Then
         Begin
            If r = huft_incomplete Then
               huft_free(tb);
            Result := unzip_ZipFileErr;
            exit
         End;

         r := GetTree(@l[0], 64);
         If r <> 0 Then
         Begin
            huft_free(tb);
            Result := unzip_ZipFileErr;
            exit
         End;

         r := huft_build(pWord(@l), 64, 0, PushList(@cplen3), PushList(@extra),
            pphuft(@tl), bl);
         If r <> 0 Then
         Begin
            If r = huft_incomplete Then
               huft_free(tl);
            huft_free(tb);
            Result := unzip_ZipFileErr;
            exit
         End;

         r := GetTree(@l[0], 64);
         If r <> 0 Then
         Begin
            huft_free(tb);
            huft_free(tl);
            Result := unzip_ZipFileErr;
            exit
         End;

         If hufttype And 2 <> 0 Then
         Begin                          {8k}
            r := huft_build(pWord(@l), 64, 0, PushList(@cpdist8), PushList(@extra),
               pphuft(@td), bd);
            If r <> 0 Then
            Begin
               If r = huft_incomplete Then
                  huft_free(td);
               huft_free(tb);
               huft_free(tl);
               Result := unzip_ZipFileErr;
               exit
            End;
            r := explode_lit8(tb, tl, td, bb, bl, bd);
         End
         Else
         Begin
            r := huft_build(pWord(@l), 64, 0, PushList(@cpdist4), PushList(@extra),
               pphuft(@td), bd);
            If r <> 0 Then
            Begin
               If r = huft_incomplete Then
                  huft_free(td);
               huft_free(tb);
               huft_free(tl);
               Result := unzip_ZipFileErr;
               exit
            End;
            r := explode_lit4(tb, tl, td, bb, bl, bd);
         End;
         huft_free(td);
         huft_free(tl);
         huft_free(tb);
      End
      Else
      Begin                             {No literal tree}

         r := GetTree(@l[0], 64);
         If r <> 0 Then
         Begin
            Result := unzip_ZipFileErr;
            exit
         End;

         r := huft_build(pWord(@l), 64, 0, PushList(@cplen2), PushList(@extra),
            pphuft(@tl), bl);
         If r <> 0 Then
         Begin
            If r = huft_incomplete Then
               huft_free(tl);
            Result := unzip_ZipFileErr;
            exit
         End;

         r := GetTree(@l[0], 64);
         If r <> 0 Then
         Begin
            huft_free(tl);
            Result := unzip_ZipFileErr;
            exit
         End;

         If hufttype And 2 <> 0 Then
         Begin                          {8k}
            r := huft_build(pWord(@l), 64, 0, PushList(@cpdist8), PushList(@extra),
               pphuft(@td), bd);
            If r <> 0 Then
            Begin
               If r = huft_incomplete Then
                  huft_free(td);
               huft_free(tl);
               Result := unzip_ZipFileErr;
               exit
            End;
            r := explode_nolit8(tl, td, bl, bd);
         End
         Else
         Begin
            r := huft_build(pWord(@l), 64, 0, PushList(@cpdist4), PushList(@extra),
               pphuft(@td), bd);
            If r <> 0 Then
            Begin
               If r = huft_incomplete Then
                  huft_free(td);
               huft_free(tl);
               Result := unzip_ZipFileErr;
               exit
            End;
            r := explode_nolit4(tl, td, bl, bd);
         End;
         huft_free(td);
         huft_free(tl);
      End;
      Result := r;
   End;

Begin
   WSIZE := 32768;
   Infile := Inf;
   Outfile := Outf;
	AdditionalBitsInTable := ABIT;

   ReadProc := InflateProc.RB;
   WriteProc := InflateProc.WB;
   ProgressProc := InflateProc.PP;
   pProgressPos := InflateProc.pProgressPos;
   pCancel := InflateProc.pCancel;

   InflateRec := IR;
	HuftType := InflateRec.BitFlag;
   Bytes_To_Go := InflateRec.UnpackedSize;

   GetMem(Slide, WSIZE);
   Try
      Result := _Explode() > 0;
   Finally
      FreeMem(Slide, WSIZE);
   End;
End;
//-------------------------------------------------------------
//-------------------------------------------------------------
//
// Explode()  ...end
//
//-------------------------------------------------------------
//-------------------------------------------------------------


End.
