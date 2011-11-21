unit ApkConsts;

interface

const
  ADB_DEVICE = 'adb devices';
  ADB_INSTALL = 'adb -s %s install %s "%s"';
  ADB_UNINSTALL = 'adb -s %s uninstall "%s"';
  ADB_MKDIR = 'adb -s %s shell mkdir "%s"';
  ADB_PUSH = 'adb -s %s push "%s" "%s"';
  ADB_PULL = 'adb -s %s pull "%s" "%s"';
  ADB_SHELL_LS_G1_G2_G3 = 'adb -s %s shell ls -F -1 "%s"';
  ADB_SHELL_LS_G4_i7500 = 'adb -s %s shell ls -l "%s"';
  ADB_SHELL_LS_APP = 'adb -s %s shell ls -l /data/app';
  ADB_SHELL_LS_APPTOSD_APP = 'adb -s %s shell ls -l /system/sd/app';
  // ADB_LS_CHECK = 'adb shell ls-F';
  ADB_REBOOT = 'adb -s %s reboot';
  ADB_SHELL_REBOOT = 'adb -s %s shell reboot';
  ADB_REBOOT_BOOTLOADER = 'adb -s %s reboot bootloader';
  ADB_REBOOT_RECOVERY = 'adb -s %s reboot recovery';
  ADB_REMOUNT = 'adb -s %s remount';
  ADB_GET_PROP = 'adb -s %s shell getprop';

  ADB_CHECK_SQLITE3_EXIST = 'adb -s %s shell sqlite3';
  ADB_CHECK_SQLITE3 = 'adb -s %s shell sqlite3 "%s" .schema';
  ADB_CHMOD = 'adb -s %s shell chmod 777 %s';
  ADB_SQLITE3_CMD = 'adb -s %s shell sqlite3 "%s" "%s"';

  ADB_SQLITE3_DUMP = '.dump <TableName>';
  ADB_SQLITE3_TABLES = '.tables';

  CMDP_R = '-r';

  LINUX_SP = '/';
  LINUX_EQUAL = '=';
  LINUX_SDCARD_SP = '/sdcard/';
  WIN32_SP = '\';
  LINUX_LNK = '@';
  LINUX_FL = '*';
  LINUX_PT = ':';
  LINUX_CA = ',';
  LINUX_LQ = '[';
  LINUX_RQ = ']';
  LINUX_UP = '&';
  WIN32_CU = '.';
  WIN32_UP = '..';

  LINUX_FMARK = '-';
  LINUX_FMARK_EX = 'x';
  LINUX_DMARK = 'd';
  LINUX_LMARK = 'l';
  LINUX_LMARK_EX = '->';

  LINUX_G3 = #$1B'[0m';
  LINUX_G3_ST = #$1B;
  LINUX_G3_SP = 'm';

  WIN32_DEVICE = ':\';
  WIN32_DISK = ':';
  

var
  ADB_EXE: string;
  ADB_PATH: string;

implementation

end.
