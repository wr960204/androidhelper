unit UIChsConsts;

interface

const
  SEL_APK = '请正确的选择要安装的文件！';
  SEL_UN_APK = '请正确选择要卸载的文件！';
  SEL_APK_PATH = '请指定 APK 文件要保存到的手机路径！';
  SEL_SEND = '请正确的选择要发送的文件！';
  SEL_SEND_PATH = '请指定文件要保存到的手机路径！';
  SEND_DIR = '发送本地文件夹到手机...';
  REBOOT_QUEST = '您的手机将重启，您确定要如此做吗？';
  BATCH_INSTALL = '您选择了一个目录，程序将搜索该目录并安装所有找到的 APK 应用程序，确定要如此做吗？';
  BATCH_UN_INSTALL = '您选择了一个目录，程序将搜索该目录并卸载所有找到的 APK 应用程序，确定要如此做吗？';
  SEL_DIR_FILE = '请正确的输入手机目录或文件名！';
  SEL_DIR_FILE_PATH = '请正确的选择要保存文件的目录！';
  SEL_APK_ERROR = '请选择一个 APK 格式的文件！';

  STR_HINT = '提示';
  STR_OP_FINISH = '本次操作完成！';
  STR_NO_DEVICE = '当前没有设备被连接到电脑。';
  STR_INSTALL_OK = '安装成功！';
  STR_INSTALL_UNKNOWN = '未知的安装状态，请检查设备！';
  STR_PERMISSION_NO = '没有足够的权限来完成此操作！';
  STR_GET_DEVICE_ERROR = '获取设备出错，请重试！';

  STR_APK_PATH = '选择 APK 的保存路径';
  STR_FILE_PATH = '选择要保存文件的目录';
  STR_M_FILE_PATH = '选择手机上文件的保存路径';
  STR_DIR_FILE = '选择要发送的文件或目录';
  STR_SAVE_FILE = '选择要保存文件的目录';
  STR_SEL_FILE = '选择文件';
  STR_SEL_APK_FILE = '选择 APK 文件';
  STR_UN_INSTLL_DIAG = '卸载应用程序';

  STR_APK_TO_INSTALL = '应用程序名称/目录';
  STR_SAVE_APK_TO_DEVICE = '将安装过的 APK 存入手机中保存';
  STR_EXIST_REINSTALL = '软件已存在,重新安装';
  STR_APK_SAVE_PATH = 'APK 保存目录';
  
  STR_INSTALLING = '正在安装，请稍候...';
  STR_INSTALLING_SHORT = '正在安装...';
  STR_UNINSTALLING = '正在卸载，请稍候...';
  STR_INSTALL = '安装';
  STR_UNINSTALL = '卸载';
  STR_REBOOT_OPT = '重启选项';
  STR_REBOOT = '重启手机';
  STR_NORMAL_REBOOT = '普通重启';
  STR_BOOTLOADER = 'BootLoader';
  STR_RECOVERY = 'Recovery';
  STR_SEL_FILE_DIR = '选择要接收的文件或目录';
  STR_PC_SAVE_PATH = 'PC 文件保存目录';
  STR_RECEIVING = '正在接收,请稍候...';
  STR_RECEIVE_TO_PC = '接收到 PC';
  STR_SEL_FILE_SEND = '选择要发送的文件或目录';
  STR_MOBILE_SAVE_PATH = '手机文件保存目录';
  STR_SENDING = '正在发送,请稍候...';
  STR_SEND_TO_MOBILE = '发送到手机';
  STR_CHECK_CONNECTED = '检查已连接到 PC 的设备';
  STR_TAB_INSTALL_APK = '安装程序';
  STR_TAB_SEND_FILE = '发送文件';
  STR_TAB_RECEIVE_FILE = '接收文件';
  STR_TAB_REBOOT = '重启手机';
  STR_TAB_HELP = '使用说明';
  STR_TAB_UI_SETTING = '界面设置';
  STR_APP_STORE = '应用程序下载';
  STR_CATEGORY = '分类';
  STR_APP_NAME = '名称';
  STR_SEARCH = '搜索';
  STR_2D_CODE = '二维识别码';

  STR_MN_CUT = '剪切';
  STR_MN_COPY = '复制';
  STR_MN_PASTE = '粘贴';
  STR_MN_DELETE = '删除';
  STR_MN_SELALL = '全选';
  STR_MN_UNDO = '撤销';

  STR_SEL_DEVICE = '选择手机设备';
  STR_SEL_CATEGORY = '选择应用程序分类';
  STR_ALL_APPS = '所有应用';
  STR_BTN_OK = '确定';
  STR_BTN_CANCEL = '取消';
  STR_BTN_CLOSE = '关闭';
  STR_BTN_BACK = '后退';
  STR_BTN_UNINSTALL = '卸载';
  STR_BTN_CURR_PATH = '当前目录';
  STR_BTN_DOWN_AND_INSTALL = '下载并安装';
  STR_BTN_EXECUTE_SQL = '执行 SQL <F9>';
  STR_BTN_SQL_OPT = '选项';
  STR_BTN_SQL_HELP = 'SQLite3 帮助';
  STR_VIEW_WEBSITE = '访问软件作者的主页';
  STR_VIEW_MAIL = '给软件作者写邮件';
  STR_UI_COLOR = '背景颜色';
  STR_UI_TRANSPARENT = '透明度';
  STR_UI_SCALE = '窗口比例';
  STR_SELECT_SQLITE_DB = '选择 SQLite3 数据库';
  STR_CREATE_NEW_DB = '输入数据库文件名：';
  STR_SQLITE3_ERR_MSG = '选定的文件不是有效的 SQLite3 数据库文件';
  STR_SELECT_ONLY_FILE = '请选择一个文件！';
  STR_SQLITE3_NOT_INSTALLED = '指定的设备上没有找到 SQLite3 模块，是否要安装它？';
  STR_SQLITE3_NO_PERMISSION = '你没有操作 SQLite3 的权限，请检查 ROOT 权限是否已获得！';
  
  STR_HELP =
    '【安装程序】将指定的 APK 安装到手机，同时可以将该 APK 备份到手机上'#13#10+
    '【发送文件】将电脑上指定的文件或目录发送到手机'#13#10+
    '【接收文件】将手机上指定的文件或目录发送到电脑'#13#10+
    '【重启手机】重启手机，并可以按需求切换至BootLoader或Recovery模式';
  STR_NEED_ROOT = '部分功能需要用户的手机拥有 Root 权限';
  STR_JOIN_SURVEY = '参与调查';

  STR_LINUX_UP = '返回上一级&';
  STR_NA = '无';

  SM_EXISTS = '应用程序已存在';
  SM_SDK_NOT_FIT = '应用程序所用的 SDK 与机器固件不符合';
  SM_APPTOSD = '检测到 AppToSD，将检索 SD 卡';
  SM_NO_SUCH = '不存在指定的文件或目录';
  SM_IS_DIR = '目录出错';

  FMT_COPY_FAIL = '复制文件失败：原因：%s';
  FMT_CREATE_FAIL = '创建目录失败：原因：%s';
  FMT_INSTALL_FAIL = '%s 安装失败，原因：%s';
  FMT_REBOOT_FAIL = '重启手机失败，原因：%s';
  FMT_RECEIVE_FAIL = '接收文件失败：原因：%s';
  FMT_DEV_CONN = '设备 %s 已连接到电脑';
  FMT_N_DEV_CONN = '检测到 %d 个设备已连接到电脑，这个情况下将无法操作，请拨下一些设备使当前连接的设备只有一个。';
  FMT_DEVICE = '当前操作的设备: %s';
  FMT_UNINSTALL_SUCCESS = '%s 卸载成功！';
  FMT_UNINSTALL_FAILURE = '%s 卸载失败！';
  FMT_UNINSTALL_ERR = '卸载过程中失去响应，请重试！';
  FMT_SIZE = '尺寸: %s 字节';

  FMT_APP_SIZE = '软件尺寸: %s';
  FMT_APP_DOWN_CNT = '下载次数: %d';
  FMT_APP_DATE = '发布日期: %s';
  FMT_APP_PLATFORM = '适用平台: %s';
  FMT_APP_AUTHOR = '软件作者: %s';

  FMT_INSTALL_MSG = '%s 安装完毕！';

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

  STR_HINT_WIN32_DESK = '切换到桌面文件夹';
  STR_HINT_WIN32_HOME = '切换到我的电脑';
  STR_HINT_WIN32_DOCUMENT = '切换到我的文档';
  STR_HINT_LINUX_HOME = '切换到 Home';
  STR_HINT_LINUX_SDCARD = '切换到 SD 卡';
  STR_HINT_LINUX_NEW_DB = '新建 SQLite3 数据库';
  STR_HINT_DELAPK = '直接卸载选中的应用程序';
  STR_HINT_APP_STORE = '访问软件中心并下载安装';
  STR_HINT_SQLITE = '操作 Android 内置的 SQLite3';
  STR_HINT_SET_LEVEL = '点击可以对此应用程序打分';
  STR_HINT_DUMP = '输出数据库 / 表的数据脚本';
  STR_HINT_TABLES = '输出表名列表';
  STR_HINT_LOAD_SCRIPT = '加载 SQLite3 脚本';

  STRARR_CATEGORY : array[0..7] of string = (
    '所有应用','通讯','娱乐','财经','健康','体育','游戏','工具');
  
implementation

end.
