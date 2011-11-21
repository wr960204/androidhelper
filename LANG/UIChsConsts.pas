unit UIChsConsts;

interface

const
  SEL_APK = '����ȷ��ѡ��Ҫ��װ���ļ���';
  SEL_UN_APK = '����ȷѡ��Ҫж�ص��ļ���';
  SEL_APK_PATH = '��ָ�� APK �ļ�Ҫ���浽���ֻ�·����';
  SEL_SEND = '����ȷ��ѡ��Ҫ���͵��ļ���';
  SEL_SEND_PATH = '��ָ���ļ�Ҫ���浽���ֻ�·����';
  SEND_DIR = '���ͱ����ļ��е��ֻ�...';
  REBOOT_QUEST = '�����ֻ�����������ȷ��Ҫ�������';
  BATCH_INSTALL = '��ѡ����һ��Ŀ¼������������Ŀ¼����װ�����ҵ��� APK Ӧ�ó���ȷ��Ҫ�������';
  BATCH_UN_INSTALL = '��ѡ����һ��Ŀ¼������������Ŀ¼��ж�������ҵ��� APK Ӧ�ó���ȷ��Ҫ�������';
  SEL_DIR_FILE = '����ȷ�������ֻ�Ŀ¼���ļ�����';
  SEL_DIR_FILE_PATH = '����ȷ��ѡ��Ҫ�����ļ���Ŀ¼��';
  SEL_APK_ERROR = '��ѡ��һ�� APK ��ʽ���ļ���';

  STR_HINT = '��ʾ';
  STR_OP_FINISH = '���β�����ɣ�';
  STR_NO_DEVICE = '��ǰû���豸�����ӵ����ԡ�';
  STR_INSTALL_OK = '��װ�ɹ���';
  STR_INSTALL_UNKNOWN = 'δ֪�İ�װ״̬�������豸��';
  STR_PERMISSION_NO = 'û���㹻��Ȩ������ɴ˲�����';
  STR_GET_DEVICE_ERROR = '��ȡ�豸���������ԣ�';

  STR_APK_PATH = 'ѡ�� APK �ı���·��';
  STR_FILE_PATH = 'ѡ��Ҫ�����ļ���Ŀ¼';
  STR_M_FILE_PATH = 'ѡ���ֻ����ļ��ı���·��';
  STR_DIR_FILE = 'ѡ��Ҫ���͵��ļ���Ŀ¼';
  STR_SAVE_FILE = 'ѡ��Ҫ�����ļ���Ŀ¼';
  STR_SEL_FILE = 'ѡ���ļ�';
  STR_SEL_APK_FILE = 'ѡ�� APK �ļ�';
  STR_UN_INSTLL_DIAG = 'ж��Ӧ�ó���';

  STR_APK_TO_INSTALL = 'Ӧ�ó�������/Ŀ¼';
  STR_SAVE_APK_TO_DEVICE = '����װ���� APK �����ֻ��б���';
  STR_EXIST_REINSTALL = '����Ѵ���,���°�װ';
  STR_APK_SAVE_PATH = 'APK ����Ŀ¼';
  
  STR_INSTALLING = '���ڰ�װ�����Ժ�...';
  STR_INSTALLING_SHORT = '���ڰ�װ...';
  STR_UNINSTALLING = '����ж�أ����Ժ�...';
  STR_INSTALL = '��װ';
  STR_UNINSTALL = 'ж��';
  STR_REBOOT_OPT = '����ѡ��';
  STR_REBOOT = '�����ֻ�';
  STR_NORMAL_REBOOT = '��ͨ����';
  STR_BOOTLOADER = 'BootLoader';
  STR_RECOVERY = 'Recovery';
  STR_SEL_FILE_DIR = 'ѡ��Ҫ���յ��ļ���Ŀ¼';
  STR_PC_SAVE_PATH = 'PC �ļ�����Ŀ¼';
  STR_RECEIVING = '���ڽ���,���Ժ�...';
  STR_RECEIVE_TO_PC = '���յ� PC';
  STR_SEL_FILE_SEND = 'ѡ��Ҫ���͵��ļ���Ŀ¼';
  STR_MOBILE_SAVE_PATH = '�ֻ��ļ�����Ŀ¼';
  STR_SENDING = '���ڷ���,���Ժ�...';
  STR_SEND_TO_MOBILE = '���͵��ֻ�';
  STR_CHECK_CONNECTED = '��������ӵ� PC ���豸';
  STR_TAB_INSTALL_APK = '��װ����';
  STR_TAB_SEND_FILE = '�����ļ�';
  STR_TAB_RECEIVE_FILE = '�����ļ�';
  STR_TAB_REBOOT = '�����ֻ�';
  STR_TAB_HELP = 'ʹ��˵��';
  STR_TAB_UI_SETTING = '��������';
  STR_APP_STORE = 'Ӧ�ó�������';
  STR_CATEGORY = '����';
  STR_APP_NAME = '����';
  STR_SEARCH = '����';
  STR_2D_CODE = '��άʶ����';

  STR_MN_CUT = '����';
  STR_MN_COPY = '����';
  STR_MN_PASTE = 'ճ��';
  STR_MN_DELETE = 'ɾ��';
  STR_MN_SELALL = 'ȫѡ';
  STR_MN_UNDO = '����';

  STR_SEL_DEVICE = 'ѡ���ֻ��豸';
  STR_SEL_CATEGORY = 'ѡ��Ӧ�ó������';
  STR_ALL_APPS = '����Ӧ��';
  STR_BTN_OK = 'ȷ��';
  STR_BTN_CANCEL = 'ȡ��';
  STR_BTN_CLOSE = '�ر�';
  STR_BTN_BACK = '����';
  STR_BTN_UNINSTALL = 'ж��';
  STR_BTN_CURR_PATH = '��ǰĿ¼';
  STR_BTN_DOWN_AND_INSTALL = '���ز���װ';
  STR_BTN_EXECUTE_SQL = 'ִ�� SQL <F9>';
  STR_BTN_SQL_OPT = 'ѡ��';
  STR_BTN_SQL_HELP = 'SQLite3 ����';
  STR_VIEW_WEBSITE = '����������ߵ���ҳ';
  STR_VIEW_MAIL = '���������д�ʼ�';
  STR_UI_COLOR = '������ɫ';
  STR_UI_TRANSPARENT = '͸����';
  STR_UI_SCALE = '���ڱ���';
  STR_SELECT_SQLITE_DB = 'ѡ�� SQLite3 ���ݿ�';
  STR_CREATE_NEW_DB = '�������ݿ��ļ�����';
  STR_SQLITE3_ERR_MSG = 'ѡ�����ļ�������Ч�� SQLite3 ���ݿ��ļ�';
  STR_SELECT_ONLY_FILE = '��ѡ��һ���ļ���';
  STR_SQLITE3_NOT_INSTALLED = 'ָ�����豸��û���ҵ� SQLite3 ģ�飬�Ƿ�Ҫ��װ����';
  STR_SQLITE3_NO_PERMISSION = '��û�в��� SQLite3 ��Ȩ�ޣ����� ROOT Ȩ���Ƿ��ѻ�ã�';
  
  STR_HELP =
    '����װ���򡿽�ָ���� APK ��װ���ֻ���ͬʱ���Խ��� APK ���ݵ��ֻ���'#13#10+
    '�������ļ�����������ָ�����ļ���Ŀ¼���͵��ֻ�'#13#10+
    '�������ļ������ֻ���ָ�����ļ���Ŀ¼���͵�����'#13#10+
    '�������ֻ��������ֻ��������԰������л���BootLoader��Recoveryģʽ';
  STR_NEED_ROOT = '���ֹ�����Ҫ�û����ֻ�ӵ�� Root Ȩ��';
  STR_JOIN_SURVEY = '�������';

  STR_LINUX_UP = '������һ��&';
  STR_NA = '��';

  SM_EXISTS = 'Ӧ�ó����Ѵ���';
  SM_SDK_NOT_FIT = 'Ӧ�ó������õ� SDK ������̼�������';
  SM_APPTOSD = '��⵽ AppToSD�������� SD ��';
  SM_NO_SUCH = '������ָ�����ļ���Ŀ¼';
  SM_IS_DIR = 'Ŀ¼����';

  FMT_COPY_FAIL = '�����ļ�ʧ�ܣ�ԭ��%s';
  FMT_CREATE_FAIL = '����Ŀ¼ʧ�ܣ�ԭ��%s';
  FMT_INSTALL_FAIL = '%s ��װʧ�ܣ�ԭ��%s';
  FMT_REBOOT_FAIL = '�����ֻ�ʧ�ܣ�ԭ��%s';
  FMT_RECEIVE_FAIL = '�����ļ�ʧ�ܣ�ԭ��%s';
  FMT_DEV_CONN = '�豸 %s �����ӵ�����';
  FMT_N_DEV_CONN = '��⵽ %d ���豸�����ӵ����ԣ��������½��޷��������벦��һЩ�豸ʹ��ǰ���ӵ��豸ֻ��һ����';
  FMT_DEVICE = '��ǰ�������豸: %s';
  FMT_UNINSTALL_SUCCESS = '%s ж�سɹ���';
  FMT_UNINSTALL_FAILURE = '%s ж��ʧ�ܣ�';
  FMT_UNINSTALL_ERR = 'ж�ع�����ʧȥ��Ӧ�������ԣ�';
  FMT_SIZE = '�ߴ�: %s �ֽ�';

  FMT_APP_SIZE = '����ߴ�: %s';
  FMT_APP_DOWN_CNT = '���ش���: %d';
  FMT_APP_DATE = '��������: %s';
  FMT_APP_PLATFORM = '����ƽ̨: %s';
  FMT_APP_AUTHOR = '�������: %s';

  FMT_INSTALL_MSG = '%s ��װ��ϣ�';

  STR_SURVEY_BODY =
    '��л��ʹ�á�Android Helper��%0A%0C'+
    '�˵�����Ϊ�˰��������˽�Ŀǰ�� GPhone �г������Լ��û���ѡ������%0A%0C'+
    '�Ӷ����õ�Ϊ GPhone / Android ϵͳ����Ӧ�ö����С�%0A%0C'+
    '�����鲻���������߱�֤���Ὣ�κε������õ���Ϣ͸¶����������%0A%0C'+
    '%0A%0C'+
    '�����Ա�%0A%0C'+
    '%0A%0C'+
    '�ֻ��ͺţ�%0A%0C'+
    '%0A%0C'+
    '����ʱ�۸�%0A%0C'+
    '%0A%0C'+
    '���������%0A%0C'+
    '%0A%0C'+
    '���������룺%0A%0C'+
    '%0A%0C'+
    '����ְҵ��%0A%0C'+
    '%0A%0C'+
    '���´ι����ֻ�ʱ�����㹺��GPhone��%0A%0C'+
    '%0A%0C'+
    '������GPhone��ʲô�ŵ��ȱ�㣺%0A%0C'+
    '%0A%0C'+
    '��ϣ��GPhone����ʲôӦ�ó���%0A%0C'+
    '%0A%0C'+
    '�ٴθ�л���Ĳ��룡%0A%0C';

  STR_HINT_WIN32_DESK = '�л��������ļ���';
  STR_HINT_WIN32_HOME = '�л����ҵĵ���';
  STR_HINT_WIN32_DOCUMENT = '�л����ҵ��ĵ�';
  STR_HINT_LINUX_HOME = '�л��� Home';
  STR_HINT_LINUX_SDCARD = '�л��� SD ��';
  STR_HINT_LINUX_NEW_DB = '�½� SQLite3 ���ݿ�';
  STR_HINT_DELAPK = 'ֱ��ж��ѡ�е�Ӧ�ó���';
  STR_HINT_APP_STORE = '����������Ĳ����ذ�װ';
  STR_HINT_SQLITE = '���� Android ���õ� SQLite3';
  STR_HINT_SET_LEVEL = '������ԶԴ�Ӧ�ó�����';
  STR_HINT_DUMP = '������ݿ� / ������ݽű�';
  STR_HINT_TABLES = '��������б�';
  STR_HINT_LOAD_SCRIPT = '���� SQLite3 �ű�';

  STRARR_CATEGORY : array[0..7] of string = (
    '����Ӧ��','ͨѶ','����','�ƾ�','����','����','��Ϸ','����');
  
implementation

end.
