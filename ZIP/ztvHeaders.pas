Unit ztvHeaders;

Interface


Uses
   Windows,
   SysUtils,
   ztvGbls,
   ztvConsts;

Type
   Ttech = Record
      TYP: Byte;
      Qual: Byte;
      PARM: Word;
   End;

   { 7 bytes }
   TAceHead = Packed Record
      HEAD_CRC: Word;
      HEAD_SIZE: Word;
      HEAD_TYPE: Byte;
      HEAD_FLAGS: Word;
      //ADDSIZE: Integer;
      //other: pchar
   End;

   TAceMHead = Packed Record
      AC: TAceHead;
      AceSign: Array[0..6] Of Char;
      VER_MOD: Byte;
      VER_CR: Byte;
      HOST_CR: Byte;
      VOL_NUM: Byte;
      TIME_CR: Integer;
      RES1: Word;
      RES2: Word;
      res: Integer;
      AV_SIZE: Byte;
      //AV: PChar;
      //COMM_SIZE: Word;
      //COMM: pchar;
   End;

   TAceFHead = Packed Record
      AC: TAceHead;
      pSize: DWord;
      size: DWord;
      FTIME: Integer;
      Attr: Integer;
      CRC32: u_long;
      tech: Ttech;
      Reserved: Word;
      FNAME_SIZE: Word;
      //FNAME: pchar;
      //COMM_SIZE: Word;
      //COMM: pchar;
   End;

   TARJHEADER = Packed Record
      HeadId: Word;
      HdrSize: Word;
      HeadSize: Byte;
      VerNum: Byte;
      MinVerNum: Byte;
      HostOS: Byte;
      ArjFlag: Byte;
      CompressType: Byte;
      FileType: Byte;
      xor_value: Byte;
      FileDate: Integer;
      PackedSize: fs_long;
      UnpackedSize: fs_long;
      CRC32: u_long;
      FilePosF: Word;
      ExternalAttr: Word;
      HostData: Word;
      //FileName (null terminated)
      //Comment  (null terminated)
      //ExtendedHeadSize:  Word
      //HeaderCRC: u_long
      //HdrFlag: Byte
   End;

   TBh = Packed Record
      SignAtr: Integer;
      HdrSize: Word;
      HeadSize: Byte;                 	{ value is assigned but currently not used }
      VerNum: Byte;
      MinVerNum: Byte;
      BitFlag: Byte;
      CompressType: Byte;
      FileDate: Integer;
      PackedSize: fs_long;
      UnpackedSize: fs_long;
      CRC32: u_long;
      ExternalAttr: Word;
      HeadCrc32: u_long;
      FileNameLen: Word;
      CommentLen: Word;
   End;

   TCAB_FILE_HEADER = Packed Record    { v4.1.7 renamed from TCAB_HEADER     				  }
      Signature: Integer;              { cabinet file signature                          }
      reserved1: Integer;              { reserved                                        }
      cbCabinet: Integer;              { size of this cabinet file in bytes              }
      reserved2: Integer;              { reserved                                        }
      coffFiles: Integer;              { offset of the first CFFILE entry                }
      reserved3: Integer;              { reserved                                        }
      versionMinor: Byte;              { cabinet file format version, minor              }
      versionMajor: Byte;              { cabinet file format version, major              }
      cFolders: Word;                  { number of CFFOLDER entries in this cabinet      }
      cFiles: Word;                    { number of CFFILE entries in this cabinet        }
      flags: Word;                     { cabinet file option indicators                  }
      setID: Word;                     { must be the same for all cabinets in a set      }
      iCabinet: Word;                  { number of this cabinet file in a set            }
      //cbCFHeader: Word; 	            { (optional) size of per-cabinet reserved area    }
      //cbCFFolder: Byte;              { (optional) size of per-folder reserved area     }
      //cbCFData: Byte;		            { (optional) size of per-datablock reserved area  }
      //abReserve: String; 	         { (optional) per-cabinet reserved area            }
   End;

   TCAB_FOLDER = Packed Record
      coffCabStart: Integer;           { offset of the first CFDATA block in this folder }
      cCFData: Word;                   { number of CFDATA blocks in this folder          }
      CompressType: Word;              { compression type indicator                      }
      //abReserve: Byte;	    	      { (optional) per-folder reserved area             }
   End;

   TCAB_CFFILE = Packed Record
      UnpackedSize: fs_long;           {Cardinal v4.0   // uncompressed size of this file in bytes}
      uoffFolderStart: fs_long;        {Cardinal v4.0   // uncompressed offset of this file in the folder}
      iFolder: Word;                   { index into the CFFOLDER area 	}
      FileDate: Integer;               { date/time stamp for this file  }
      ExternalAttr: Word;              { attribute flags for this file  }
      //szName: String;	              	{ name of this file              }
   End;

   TCAB_CFDATA = Packed Record
      csum: Integer;                   { checksum of this CFDATA entry					}
      cbData: Word;                    { number of compressed bytes in this block   }
      cbUncomp: Word;                  { number of uncompressed bytes in this block }
      //abReserve: Byte;	          	{ (optional) per-datablock reserved area     }
      //ab: ARRAY[0..cbData-1] Of Byte;{ compressed data bytes                      }
   End;

   TGZipHeader = Packed Record
      SignAtr: Word;
      CompressType: Byte;
      BitFlag: Byte;
      	{ Bit 0: fTEXT    compressed file contains text, can be used for			}
         {                 cross platform line termination translation           }
      	{ Bit 1: fHCRC    header data includes CRC16 for header after           }
         {                 Extra Data, FileName, and Comment                     }
      	{ Bit 2: fEXTRA   header data contains Extra Data, starts immediately   }
         {                 after header proper                                   }
      	{ Bit 3: fNAME    header data contains FileName, null terminated        }
         {             	   string starting immediately after Extra Data (if any) }
      	{ Bit 4: fCOMMENT header data contains Comment, null terminated string  }
         {             	   starting immediately after FileName (if any)          }
      	{ Bits 5..7       reserved                                              }
      FileDate: Integer;
      ExtraFlags: Byte;
      	{ ExtraFlags = 2  -- Deflate compressor used maximum compression algorithm }
      	{ ExtraFlags = 4  -- Deflate compressor used fastest algorithm 				}
      OS: Byte;
         {   0 = FAT				}
         {   1 = ID_Amiga     }
         {   2 = VMS          }
         {   3 = Unix         }
         {   4 = CMS          }
         {   5 = AtariTOS     }
         {   6 = HPFS         }
         {   7 = Macintosh    }
         {   8 = Z_System     }
         {   9 = CP_M         }
         {  10 = TOPS20       }
         {  11 = NTFS         }
         {  12 = QDOS         }
         {  13 = AcornRISCOS  }
         { 255 = unknown      }
      FileNameLen: Integer;
   End;

   THA = Packed Record
      VType: Byte;
      VerNum: Byte;
      PackedSize: fs_long;
      UnpackedSize: fs_long;
      CRC32: u_long;
      FileDate: Integer;
      Path: String;
      CompressedFileName: String;
      mdiLen: Word;
      mylen: Word;
      //--------------------
      CompressType: Byte;
   End;

   TLzhExtended = Packed Record
      CRC16: Word;
      Confirm: Byte;                   { constant of 0x20									}
      ExtHeaderSize: Word;             { 2	first extended header size (0 if none) }
   End;

   TLzh = Packed Record
      HeadLen: Byte;                   { 0 = end of file 				}
      Headchk: Byte;                   { checksum of remaining bytes }
      SignBegin: Byte;                 { '-' Char                    }
      l: Byte;                         { 'l' Char                    }
      H: Byte;                         { 'h' Char                    }
      CompressType: Byte;              { method                      }
      SignEnd: Byte;                   { should always be a '-' Char }
      PackedSize: fs_long;
      UnpackedSize: fs_long;
      FileDate: Integer;
      ExternalAttr: Byte;
      level: Byte;
      FileNameLen: Byte;
      LzhFileName: String;
      CRC16: Word;
      Confirm: Byte;                   { constant of 0x20            }
      ExtHeaderSize: Word;             { 2	first extended header size (0 if none) }
      ExtHeader: String;
      OffsetToCompressed: Byte;
      OS: Byte;
   End;

   TPak = Packed Record
      marker: Byte;
      CompressType: Byte;
      aFilename: Array[1..13] Of Char;
      PackedSize: fs_long;
      FileDate: Integer;
      CRC16: Word;
      UnpackedSize: fs_long;
   End;
   TARC = TPak;                        { ARC headers are the equal to PAK }

   TRar1 = Packed Record
      HeadCRC: Word;
      HeadType: Byte;
      HeadFlags: Word;
      HeadSize: Word;
      CommentLen: Byte;
      Reserved: Word;
      Reserved1: Cardinal;
      //Reserved: Array[0..4] Of Byte;
   End;

   TRarHeader = Packed Record
      HeadCRC: Word;
      HeadType: Byte;
      HeadFlags: Word;
      HeadSize: Word;
      PackedSize: fs_long;
      UnpackedSize: fs_long;
      HostOS: Byte;                    { 0 dos 1 os/2 }
      CRC32: u_long;
      FileDate: Integer;
      VerNum: Byte;
      CompressType: Byte;
      FileNameLen: Word;
      ExternalAttr: Integer;
   End;

   TRarHugeFile = Packed Record
   	HighPackedSize: Integer;
      HighUnpackedSize: Integer;
   End;

   TRARHeadData = Packed Record
      ArcName: Array[1..260] Of Char;
      FileName: Array[1..260] Of Char;
      HeadFlags: Cardinal;
      PackedSize: Cardinal;
      UnpackedSize: Cardinal;
      HostOS: Cardinal;
      CRC32: Cardinal;
      FileDate: Cardinal;
      UnpVer: Cardinal;
      CompressType: Cardinal;
      FileAttr: Cardinal;
      CmtBuf: PChar;
      CmtBufSize: Cardinal;
      CmtSize: Cardinal;
      CmtState: Cardinal;
      HighPackedSize: Cardinal;
      HighUnpackedSize: Cardinal;
   End;

   TRARHeadDataEx = Packed Record
      ArcName: Array[0..1023] Of Char;
      ArcNameW: Array[0..1023] Of WideChar;
      FileName: Array[0..1023] Of Char;
      FileNameW: Array[0..1023] Of WideChar;  
      HeadFlags: Cardinal;
      PackedSize: Cardinal;
      PackedSizeHigh: Cardinal;
      UnpackedSize: Cardinal;
      UnpackedSizeHigh: Cardinal;
      HostOS: Cardinal;
      CRC32: Cardinal;
      FileDate: Cardinal;
      UnpVer: Cardinal;
      CompressType: Cardinal;
      FileAttr: Cardinal;
      CmtBuf: PChar;
      CmtBufSize: Cardinal;
      CmtSize: Cardinal;
      CmtState: Cardinal;
      Reserved: Array[0..1023] Of Cardinal;
   End;

   TRarCommentHeader = Packed Record
  		HeadCRC: Word;
  		HeadType: Byte;
  		Flags: Word;
  		HeadSize: Word;
  		UnpSize: Cardinal;
  		UnpVer: Byte;
  		Method: Byte;
  		CommCRC: Cardinal;
   End;

   TTarHeader = Packed Record              	{ offset  	length}
      TarFilename: Array[0..99] Of Char;    	{   0     	100   }
      mode: Array[0..7] Of Char;            	{ 100     	  8   }
      UID: Array[0..7] Of Char;              { 108     	  8   }
      GID: Array[0..7] Of Char;        		{ 116     	  8   }
      size: Array[0..11] Of Char;      		{ 124     	 12   }
      MTime: Array[0..11] Of Char;     		{ 136     	 12   }
      ChkSum: Array[0..7] Of Char;     		{ 148     	  8   }
      LinkFlag: Char;                  		{ 156     	  1   }
      LinkName: Array[0..99] Of Char;  		{ 157     	100   }
      Magic: Array[0..7] Of Char;            { 257     	  8   }
      UName: Array[0..31] Of Char;           { 265     	 32   }
      GName: Array[0..31] Of Char;				{ 297     	 32   }
      DevMajor: Array[0..7] Of Char;         { 329     	  8   }
      DevMinor: Array[0..7] Of Char;         { 337     	  8   }
      Prefix{Filler}: Array[0..166] Of Char; { 345     	167   }
   End;                                      { =============   }
                                             {           512   }

   // http://nscp.upenn.edu/aix4.3html/files/aixfiles/tar.h.htm
   TExtendedTarHeader = Packed Record       	{ offset  	length  	description									}
      TarFilename: Array[0..99] Of Char;    	{   0     	100		File name without a / (slash)       }
      mode: Array[0..7] Of Char;            	{ 100     	  8      File mode                           }
      UID: Array[0..7] Of Char;              { 108     	  8		User ID                             }
      GID: Array[0..7] Of Char;        		{ 116     	  8      Group ID                            }
      size: Array[0..11] Of Char;      		{ 124     	 12      Size in bytes                       }
      MTime: Array[0..11] Of Char;     		{ 136     	 12      Latest modification time            }
      ChkSum: Array[0..7] Of Char;     		{ 148     	  8      File and header checksum            }
      TypeFlag: Char;                  		{ 156     	  1      File type                           }
      LinkName: Array[0..99] Of Char;  		{ 157     	100      Linked path name or file name       }
      Magic: Array[0..5] Of Char;            { 257     	  6		Format representation for tar       }
      Version: Array[0..1] Of Char;				{ 263  		  2		Version representation for tar      }
      UName: Array[0..31] Of Char;           { 265     	 32		User name                           }
      GName: Array[0..31] Of Char;				{ 297     	 32		Group name                          }
      DevMajor: Array[0..7] Of Char;         { 329     	  8      Major device representation         }
      DevMinor: Array[0..7] Of Char;         { 337     	  8		Minor device representation         }
      Prefix{Filler}: Array[0..154] Of Char; { 345     	155		Path name without trailing slashes  }
   End;                                      {=============                                           }
                                             {           500                                          }
   TZooDir = Packed Record
      ZooFilename: Array[1..20] Of Char;
      SignAtr: Integer;
      START: Integer;
      Minus: Integer;
      VerNum: Byte;                    { Minor }
      MinVerNum: Byte;                 { Major }
      SignAt: Byte;
      CommentPos: Integer;
      CommentLen: Word;
      udata: Word;
   End;

   TZooDirEntry = Packed Record
      lo_tag: Word;
      hi_tag: Word;
      SignAtr: Byte;
      CompressType: Byte;              { 0 = no packing, 1 = normal LZW		}
      Next: Integer;                   { pos'n of next directory entry     }
      Offset: Integer;                 { position of this file             }
      FileDate: Integer;               { DOS format date/time              }
      CRC16: Word;                     { CRC of this file                  }
      UnpackedSize: fs_long;
      PackedSize: fs_long;
      VerNum: Byte;
      MinVerNum: Byte;                 { minimum version needed to extract }
      deleted: Byte;                   { 1 if deleted, 0 if not            }
      structr: Byte;
      CommentPos: Integer;             { points to comment;  zero if none  }
      CommentLen: Word;                { length of comment, 0 if none      }
      ZooFilename: Array[1..13] Of Char;
      Var_dir_len: Word;
      tz: Byte;
      dir_crc: Word;
      lfnamelen: Byte;
      DirLen: Byte;
      lfname: Array[0..255] Of Char;
      DirName: Array[0..255] Of Char;
      system_id: Word;
      fattr: Integer;
      vflag: Word;
      version_no: Word;
   End;

   { ===============================================================					}
   { PackedSize & UnpackedSize info                                              }
   { value * MAXDWORD                                                            }
   { MAXDWORD = 4294967295                                                       }
   { Max uncompressed size (bytes):                                              }
   {    SizeOf(Byte) * MAXDWORD = 1,099,511,627,520 (one Terabyte - 1000gb)      }
   {                                                                             }
   { The following record is an ExtraField stored after a TLocal & TCentral      }
   { headers... if a file's PackedSize or UnpackedSize values exceed 4 gig       }
   { (MAXDWORD).  The TLocal.PackedSize, TLocal.UnpackedSize,                    }
   { TCentral.PackedSize and TCentralUnpackedSize variables will contain the     }
   { low order value of the file's size.  The variables corresponding variable   }
   { in this record stores the high order value of a 64-bit Integer.             }
   { ===============================================================             }
   TExtendedFieldHdr = Packed Record
   	HeadID: Word;							{ $001 = ZipTV Zip64 header }
      HeadSize: Word;
	End;

   pLocal64Hdr = ^TLocal64Hdr;
   TLocal64Hdr = Packed Record
   	ExtendedFieldHdr: TExtendedFieldHdr;
      HiPackedSize: Word;
      HiUnpackedSize: Word;
   End;

   pCentral64Hdr = ^TCentral64Hdr;
   TCentral64Hdr = Packed Record
   	ExtendedFieldHdr: TExtendedFieldHdr;
      HiPackedSize: Word;
      HiUnpackedSize: Word;
      HiOffsetToLocal: Word;
   End;

   { 24 bytes}
   Tzc = Packed Record
      BitFlag: Word;
      CompressType: Word;
      FileDate: Integer;
      CRC32: u_long;
      PackedSize: fs_long;
      UnpackedSize: fs_long;
      FileNameLen: Word;
      ExtraFieldLen: Word;
   End;

   { 30 bytes }
   TLocal = Packed Record
      SignAtr: Integer;
      VerNum: Word;
      ZC: Tzc;
   End;

   { 46 bytes }
   TCentral = Packed Record
      SignAtr: Integer;
      VerNum: Word;             			{ VersionMadeBy }
      MinVerNum: Word;            		{ version needed to extract }
      ZC: Tzc;
      CommentLen: Word;
      DiskNumberStart: Word;
      InternalAttr: Word;
      ExternalAttr: longint;
      RelativeOffsetOfLocalHeader: fs_long;
   End;

   { 22 bytes }
   TEnd = Packed Record
      SignAtr: Integer;
      NumberOfThisDisk: Word;
      DiskWithStartOfCentral: Word;
      EntriesOnDisk: Word;					{ TotalEntriesInCentralDirOnThisDisk }
      TotalEntries: Word;              { TotalEntriesInCentralDir }
      SizeOfCentralDir: Integer;
      CentralDirOffset: fs_long;
      CommentLen: Word;						{ ZipFileCommentLen }
   End;

   { 20 bytes }
	TWZipLocator = Packed Record
      SignAtr: Cardinal;               { $07064b50 }
      DiskWithStartOfCentral: Cardinal;
      RelativeOffsetOfCentralHeader: Int64;
      TotalNumberOfDisks: Cardinal;
   End;

   // 30 bytes
   TZipTV_End64 = Packed Record
      SignAtr: Integer;
      NumberOfThisDisk: Word;
      DiskWithStartOfCentral: Word;
      EntriesOnDisk: Word;             { TotalEntriesInCentralDirOnThisDisk }
      TotalEntries: Word;              { TotalEntriesInCentralDir }
      SizeOfCentralDir: Integer;
      CentralDirOffset: Int64;
      CommentLen: Word;                { ZipFileCommentLen }
   End;

   { 56 bytes }
   TWZip_END64 = Packed Record
      SignAtr: Cardinal;               { $06064b50 }
      SizeOf_TWZIP_END64: Int64;
      VersionMadeBy: Word;
      VersionRequired: Word;
      NumberOfThisDisk: Cardinal;
      DiskWithStartOfCentral: Cardinal;
      EntriesInCentralDirThisDisk: Int64;
      EntriesInCentralDirTotal: Int64;
      SizeOfCentralDir: Int64;
      RelativeOffsetOfCentralHeader: Int64;  { with respect to the starting disk number }
      //ExtensibleDataSector    			{ variable size }
   End;

   TMsGZ_DIR = Packed Record
      FilesInDir: Word;
      HeaderLen: Word; //Byte;
      //Reserved: Byte;
      DirnameLen: Word;
   End;

   TMSGZ = Packed Record
      DirectoryIndex: Word;
      UnpackedSize: fs_long;
      PackedSize: fs_long;
      DataOffset: fs_long;
      FileDate: Integer;
      ExternalAttr: Integer;
      HeaderLen: Word;
      reserved25: Byte;
      reserved26: Byte;
      reserved27: Byte;
      reserved28: Byte;
      FileNameLen: Byte;
   End;

   TMSGZMain = Packed Record
      SignAtr: Integer;
      Reserved5: Byte;
      Reserved6: Byte;
      Reserved7: Byte;
      Reserved8: Byte;
      Reserved9: Byte;
      Reserved10: Byte;
      Reserved11: Byte;
      Reserved12: Byte;
      NumberOfFiles: Byte;
      Reserved13: Byte;
      Reserved14: Byte;
      Reserved15: Byte;
      Reserved16: Byte;
      Reserved17: Byte;
      Reserved18: Byte;
      Reserved19: Byte;
      Reserved20: Byte;
      Reserved21: Byte;
      Reserved22: Byte;
      Reserved23: Byte;
      Reserved24: Byte;
      reserved25: Byte;
      HeaderLen: Byte;
      reserved28: Byte;
      Reserved29: Byte;
      Reserved30: Byte;
      Reserved31: Byte;
      Reserved32: Byte;
      Reserved33: Byte;
      Reserved34: Byte;
      Reserved35: Byte;
      Reserved36: Byte;
      Reserved37: Byte;
      Reserved38: Byte;
      Reserved39: Byte;
      Reserved40: Byte;
      Reserved41: Byte;
      OffsetToDirectory: DWord;
      Reserved46: Byte;
      Reserved47: Byte;
      Reserved48: Byte;
      Reserved49: Byte;
      Reserved50: Byte;
      Reserved51: Byte;
      Reserved52: Byte;
      Reserved53: Byte;
      Reserved54: Byte;
   End;

   TOlMain = Packed Record
      SignAtr: Integer;
      reserved1: Integer;
      EntriesOnDisk: Integer;
   End;

   TOl = Packed Record
      SignAtr: Integer;
      NextOffset: Integer;
      RecordLen: Integer;
   End;

Implementation

End.
