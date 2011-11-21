unit StringConsts;

interface

const
  STR_DOT = '...';
  STR_MASK = '*.*';
  STR_MASK_APK = '*.apk';
  STR_MASK_XML = '*.xml';
  STR_MASK_MANIFEST_XML = 'AndroidManifest.xml';
  STR_APK_EXT = '.apk';
  STR_PNG_EXT = 'png';

  STR_TITLE = 'Android Helper v2.1.22';
  V_ADEVICE_PATH = 'ADevicePath';
  V_AFILENAME = 'AFileName';
  V_APATH = 'APath';
  V_DEVICE = 'device';
  V_INI = '.ini';
  V_EXENAME = 'adb.exe';

  RE_PERMISSION_DENIED = 'permission denied';
  RE_FAILED = 'FAILED';
  RE_KILLING = 'killing';
  RE_STAR = '*';
  RE_OF = ' of ';
  RE_OFFLINE = 'offline';
  RE_FILE_EXISTS = 'File exists';
  RE_SUCCESS = 'SUCCESS';
  RE_FAILURE = 'FAILURE';
  RE_BYTES = 'bytes';
  RE_KB = 'KB';
  RE_NO_SUCH = 'No such';
  RE_IS_DIR = 'Is a directory';
  RE_ERROR = 'error';
  RE_F_NO_SUCH = '-F: No such';
  RE_CLOSED = 'closed';
  RE_EXISTS = 'INSTALL_FAILED_ALREADY_EXISTS';
  RE_SDK = 'SDK';
  RE_APPTOSD = 'app -> /system/sd/app';
  RE_SQLITE3_ERR = 'Error: file is encrypted or is not a database';
  RE_SQLITE3_NOT_FOUND = 'sqlite3: not found';
  RE_SQLITE_NO_PERMISSION = 'sqlite3: permission denied';
  FMT_ITEM_VALUE = '=%d';
  FMT_PERCENT = '%d%%';
  FMT_SQLITE3_TITLE = 'SQLite3: %s';
  FMT_QUOTED = ' (%s) ';

  FMT_APP_MAIL_TO_CMD = 'mailto:%s';
  CFG_CONFIG = 'Config';
  CFG_SAVE_APK = 'SaveApk';
  CFG_SAVE_APK_PATH = 'SaveApkPath';
  CFG_SEND_PATH = 'SendPath';
  CFG_RECEIVE_FILE = 'ReceiveFile';
  CFG_RECEIVE_PATH = 'ReceivePath';
  CFG_DEVICE_TYPE = 'DeviceType';
  CFG_FIRST_RUN = 'FirstRun';
  CFG_DEF_SAVE_APK = '/sdcard/apk/';
  CFG_DEF_FILES = '/sdcard/files/';
  CFG_DEF_LOCAL_FILES = 'files';
  CFG_DEF_SERVER = 'http://localhost:8080/ASServer/';
  CFG_UI_COLOR = 'UIColor';
  CFG_UI_TRANSPARENT = 'UITransparent';
  CFG_UI_SCALE = 'UIScale';
  CFG_SERVER = 'Server';

  SRV_ICON = 'icon/';
  SRV_APP = 'apps/';
  SRV_WSDL = 'ASService.asmx?WSDL';
  LOC_APK = 'apk\';
  LOC_ANDROID = 'Android\';
  A_SQLITE3 = 'sqlite3';
  A_LIB_SQLITE = 'libsqlite.so';
  A_SYSTEM_BIN = '/system/bin';
  A_SYSTEM_LIB = '/system/lib';
  A_SYSTEM_BIN_SQLITE3 = '/system/bin/sqlite3';

  STR_THREAD_MUTEX = 'AndroidThreadMutex';
  STR_MANIFEST = 'manifest';

  SHELL_CMD_OPEN = 'open';
  SHELL_CMD_MAILTO = 'mailto:rarnu1985@gmail.com?subject=Android Helper Survey&body=';
  SHELL_CMD_WEBSITE = 'http://hi.baidu.com/rarnu';
  SHELL_CMD_SQLITE3_WEBSITE = 'http://www.sqlite.org/';

  DEF_COLOR = '#E01E1E1E';

  IMG_F1 = 'f1';
  IMG_F2 = 'f2';
  IMG_F3 = 'f3';
  IMG_F4 = 'f4';
  IMG_F5 = 'f5';

  IMG_P1 = 'p1';
  IMG_P2 = 'p2';
  IMG_P3 = 'p3';
  IMG_P4 = 'p4';
  IMG_P5 = 'p5';
  IMG_P0 = 'p0';

  IMG_U1 = 'u1';
  IMG_U2 = 'u2';
  IMG_U3 = 'u3';
  IMG_U4 = 'u4';
  IMG_U5 = 'u5';
  IMG_U6 = 'u6';
  IMG_U7 = 'u7';
  IMG_U8 = 'u8';
  IMG_U9 = 'u9';
  IMG_U10 = 'u10';

  IMG_S1 = 's1';
  IMG_S2 = 's2';

  FMT_IMG_P = 'p%d';

  STR_POWER_DEBUG = 'SeDebugPrivilege';

  SPC = ' ';

  CH_ZERO = #0;
  CHAR_1 = #1;
  CHAR_9 = #9;
  CHAR_10 = #10;
  CHAR_13 = #13;
  CHAR_32 = #32;
  CHAR_CRLF = #13#10;
  VG_CHAR_0 = '0';
  VG_CHAR_DOT = '.';
  VG_CHAR_COMMA = ',';
  VG_CHAR_EQ = '=';
  VG_CHAR_QUOTE = '"';

  VG_CONTENT = 'content';
  VG_STRINGS = 'strings';
  VG_D_STRING = 'Doublicate string';
  VG_LIST_INDEX_ERR = 'list index error';
  VG_SORT_LIST_ERR = 'sorted list error';
  VG_I_CONTROLS = 'Controls';
  VG_I_ANIMATIONS = 'Animations';
  VG_I_HUD = 'HUD';
  VG_I_COLORS = 'Colors';
  VG_VSBAR = 'vscrollbar';
  VG_HSBAR = 'hscrollbar';
  VG_PQ = 'Pq';
  VG_QUALITY = 'quality';
  VG_ISOPEN = 'IsOpen';

  VG_CHAR_A = 'a';
  VG_CHAR_A_U = 'A';
  VG_CHAR_C = 'c';
  VG_CHAR_C_U = 'C';
  VG_CHAR_V = 'v';
  VG_CHAR_V_U = 'V';
  VG_CHAR_X = 'x';
  VG_CHAR_X_U = 'X';
  VG_CHAR_Z = 'z';
  VG_CHAR_Z_U = 'Z';

  VG_COLOR_TEXTBOX_FILL = '#802A8ADF';
  VG_GDIP_FILTER = '*.bmp;*.jpg;*.jpeg;*.png;*.tif;*.tiff;*.gif;*.ico';
  VG_FONT_ARIAL = 'Arial';

  VG_IMG_PNG = 'image/png';
  VG_IMG_JPG = 'image/jpeg';
  VG_IMG_BMP = 'image/bmp';
  VG_IMG_TIFF = 'image/tiff';
  VG_IMG_GIF = 'image/gif';

  VG_TYP_PNG = 'png';
  VG_TYP_JPG = 'jpg';
  VG_TYP_JPEG = 'jpeg';
  VG_TYP_BMP = 'bmp';
  VG_TYP_TIF = 'tif';
  VG_TYP_TIFF = 'tiff';
  VG_TYP_GIF = 'gif';

  VG_SUR_PNG = '.png';
  VG_SUR_JPG = '.jpg';
  VG_SUR_JPEG = '.jpeg';
  VG_SUR_BMP = '.bmp';
  VG_SUR_TIF = '.tif';
  VG_SUR_TIFF = '.tiff';
  VG_SUR_GIF = '.gif';

implementation

end.
