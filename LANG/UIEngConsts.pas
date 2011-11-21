unit UIEngConsts;

interface

const
  SEL_APK = 'Please choose a right file for installing!';
  SEL_UN_APK = 'Please choose a right file for uninstalling!';
  SEL_APK_PATH = 'Please choose the mobile path for saving APK!';
  SEL_SEND = 'Please choose the right file for sending!';
  SEL_SEND_PATH = 'Please choose the mobile path for saving files!';
  SEND_DIR = 'Send local folder to mobile phone...';
  REBOOT_QUEST = 'Your device will reboot, are you sure?';
  BATCH_INSTALL = 'You have selected a folder, will search it and install all APKs found. Are you sure?';
  BATCH_UN_INSTALL = 'You have selected a folder, will search it and uninstall all APKs found. Are you sure?';
  SEL_DIR_FILE = 'Please input the right Mobile Path or File Name!';
  SEL_DIR_FILE_PATH = 'Please choose a right path for saving files!';
  SEL_APK_ERROR = 'Please choose a file with surfix APK!';

  STR_HINT = 'Hint';
  STR_OP_FINISH = 'Operation finished!';
  STR_NO_DEVICE = 'No device connected.';
  STR_INSTALL_OK = 'Install success!';
  STR_INSTALL_UNKNOWN = 'Unknown status, please check your device!';
  STR_PERMISSION_NO = 'Not enough permission to do the operation.';
  STR_GET_DEVICE_ERROR = 'Get device error, please retry!';

  STR_APK_PATH = 'Choose APK Saving Path';
  STR_FILE_PATH = 'Choose a Folder for Saving Files';
  STR_M_FILE_PATH = 'Choose Mobile Path for Saving Files';
  STR_DIR_FILE = 'Choose Files or Folder to Send';
  STR_SAVE_FILE = 'Choose a Folder for Saving Files';
  STR_SEL_FILE = 'Choose File';
  STR_SEL_APK_FILE = 'Choose APK File';
  STR_UN_INSTLL_DIAG = 'Uninstall APK';

  STR_APK_TO_INSTALL = 'APK File / Folder';
  STR_SAVE_APK_TO_DEVICE = 'Save installed APK on the Phone';
  STR_EXIST_REINSTALL = 'Already Exists and Reinstall';
  STR_APK_SAVE_PATH = 'APK Saving Path';
  
  STR_INSTALLING = 'Installing, please wait...';
  STR_INSTALLING_SHORT = 'Installing...';
  STR_UNINSTALLING = 'Uninstalling, please wait...';
  STR_INSTALL = 'Install';
  STR_UNINSTALL = 'Uninstall';
  STR_REBOOT_OPT = 'Reboot Options';
  STR_REBOOT = 'Reboot';
  STR_NORMAL_REBOOT = 'Normal';
  STR_BOOTLOADER = 'BootLoader';
  STR_RECOVERY = 'Recovery';
  STR_SEL_FILE_DIR = 'Files to Receive';
  STR_PC_SAVE_PATH = 'Saving Path';
  STR_RECEIVING = 'Receiving, please wait...';
  STR_RECEIVE_TO_PC = 'Receive to PC';
  STR_SEL_FILE_SEND = 'Files to Send';
  STR_MOBILE_SAVE_PATH = 'Mobile Path';
  STR_SENDING = 'Sending, please wait...';
  STR_SEND_TO_MOBILE = 'Send to Mobile';
  STR_CHECK_CONNECTED = 'Check Connected Devices';
  STR_TAB_INSTALL_APK = 'Install';
  STR_TAB_SEND_FILE = 'Send';
  STR_TAB_RECEIVE_FILE = 'Receive';
  STR_TAB_REBOOT = 'Reboot';
  STR_TAB_HELP = 'Help';
  STR_TAB_UI_SETTING = 'UI Settings';
  STR_APP_STORE = 'Application Download';
  STR_CATEGORY = 'Category';
  STR_APP_NAME = 'Name';
  STR_SEARCH = 'Find';
  STR_2D_CODE = '2D BarCd';

  STR_MN_CUT = 'Cut';
  STR_MN_COPY = 'Copy';
  STR_MN_PASTE = 'Paste';
  STR_MN_DELETE = 'Delete';
  STR_MN_SELALL = 'Select All';
  STR_MN_UNDO = 'Undo';

  STR_SEL_DEVICE = 'Select Device';
  STR_SEL_CATEGORY = 'Select Category';
  STR_ALL_APPS = 'All';
  STR_BTN_OK = 'OK';
  STR_BTN_CANCEL = 'Cancel';
  STR_BTN_CLOSE = 'Close';
  STR_BTN_BACK = 'Back';
  STR_BTN_UNINSTALL = 'Uninstall';
  STR_BTN_CURR_PATH = 'Path';
  STR_BTN_DOWN_AND_INSTALL = 'Download';
  STR_BTN_EXECUTE_SQL = 'Execute SQL <F9>';
  STR_BTN_SQL_OPT = 'Options';
  STR_BTN_SQL_HELP = 'SQLite3 Help';
  STR_VIEW_WEBSITE = 'Visit Home Page';
  STR_VIEW_MAIL = 'Send a Mail';
  STR_UI_COLOR = 'Back';
  STR_UI_TRANSPARENT = 'Alpha';
  STR_UI_SCALE = 'Scale';
  STR_SELECT_SQLITE_DB = 'Choose SQLite3 database';
  STR_CREATE_NEW_DB = 'Input the database file name:';
  STR_SQLITE3_ERR_MSG = 'Selected file is NOT an available SQLite3 database.';
  STR_SELECT_ONLY_FILE = 'Please choose a file!';
  STR_SQLITE3_NOT_INSTALLED = 'Your device has not SQLite3 module, would you want to install it?';
  STR_SQLITE3_NO_PERMISSION = 'You have no permission for operating on SQLite3, please check the ROOT permission.';

  STR_HELP =
    '【安装程序】将指定的 APK 安装到手机，同时可以将该 APK 备份到手机上'#13#10+
    '【发送文件】将电脑上指定的文件或目录发送到手机'#13#10+
    '【接收文件】将手机上指定的文件或目录发送到电脑'#13#10+
    '【重启手机】重启手机，并可以按需求切换至BootLoader或Recovery模式';
  STR_NEED_ROOT = 'Some functions require ROOT permission';
  STR_JOIN_SURVEY = 'Do Survey';

  STR_LINUX_UP = 'Up One Level&';
  STR_NA = 'N/A';

  SM_EXISTS = 'APK Already Exists';
  SM_SDK_NOT_FIT = 'APK''s SDK not fit for Device';
  SM_APPTOSD = 'Detected AppToSD, will search SD Card';
  SM_NO_SUCH = 'No such file or directory';
  SM_IS_DIR = 'Directory error';

  FMT_COPY_FAIL = 'Copy file failed: %s';
  FMT_CREATE_FAIL = 'Create folder failed: %s';
  FMT_INSTALL_FAIL = 'Install %s failed: %s';
  FMT_REBOOT_FAIL = 'Reboot failed: %s';
  FMT_RECEIVE_FAIL = 'Receive file failed: %s';
  FMT_DEV_CONN = 'Device %s is connected.';
  FMT_N_DEV_CONN = 'Detected %d devices connected. Please make sure only one device connected and try again.';
  FMT_DEVICE = 'Current Device: %s';
  FMT_UNINSTALL_SUCCESS = 'Uninstall %s success!';
  FMT_UNINSTALL_FAILURE = 'Uninstall %s failed!';
  FMT_UNINSTALL_ERR = 'Lost response when uninstalling, please try again!';
  FMT_SIZE = 'Size: %s byte';

  FMT_APP_SIZE = 'Size: %s';
  FMT_APP_DOWN_CNT = 'Downloads: %d';
  FMT_APP_DATE = 'Date: %s';
  FMT_APP_PLATFORM = 'Platform: %s';
  FMT_APP_AUTHOR = 'Author: %s';

  FMT_INSTALL_MSG = 'Install %s Finished!';

  STR_SURVEY_BODY =
    '感谢您使用《Android Helper》%0A%0C'+
    '此调查是为了帮助作者了解目前的 GPhone 市场形势以及用户的选择形势%0A%0C'+
    '从而更好的为 GPhone / Android 系统开发应用而进行。%0A%0C'+
    '本调查不计名，作者保证不会将任何调查所得的信息透露给第三方。%0A%0C'+
    '%0A%0C'+
    '您的性别：%0A%0C'+
    '%0A%0C'+
    '手机型号：%0A%0C'+
    '%0A%0C'+
    '购买时价格：%0A%0C'+
    '%0A%0C'+
    '购买地区：%0A%0C'+
    '%0A%0C'+
    '您的月收入：%0A%0C'+
    '%0A%0C'+
    '您的职业：%0A%0C'+
    '%0A%0C'+
    '您下次购买手机时还打算购买GPhone吗：%0A%0C'+
    '%0A%0C'+
    '您觉得GPhone有什么优点和缺点：%0A%0C'+
    '%0A%0C'+
    '您希望GPhone出现什么应用程序：%0A%0C'+
    '%0A%0C'+
    '再次感谢您的参与！%0A%0C';

  STR_HINT_WIN32_DESK = 'Switch to Desktop';
  STR_HINT_WIN32_HOME = 'Switch to My Computer';
  STR_HINT_WIN32_DOCUMENT = 'Switch to My Documents';
  STR_HINT_LINUX_HOME = 'Switch to Home';
  STR_HINT_LINUX_SDCARD = 'Switch to SD Card';
  STR_HINT_LINUX_NEW_DB = 'Create new SQLite3 Database';
  STR_HINT_DELAPK = 'Uninstall selected APK';
  STR_HINT_APP_STORE = 'Visit APPSTORE for downloading and installing';
  STR_HINT_SQLITE = 'Operate on Android SQLite3';
  STR_HINT_SET_LEVEL = 'Click for set the level of this Application';
  STR_HINT_DUMP = 'Output Database / Tables schema script';
  STR_HINT_TABLES = 'Output Table Name List';
  STR_HINT_LOAD_SCRIPT = 'Load SQLite3 schema script';

  STRARR_CATEGORY : array[0..7] of string = (
    'All','Communicate','Entertainment','Finance','Health','Sports','Game','Tool');
  
implementation

end.
