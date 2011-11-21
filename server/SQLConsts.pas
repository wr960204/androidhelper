unit SQLConsts;

interface

const
  CONN_SQL = 'Provider=SQLOLEDB.1;Password=%s;Persist Security Info=True;User ID=%s;Initial Catalog=%s;Data Source=%s';
  CONN_ACCESS = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source="%s";Persist Security Info=False';

  SQL_GET_TOTAL_LEVEL = 'select (AppLevel*AppLevelCount) as ''LvCnt'',AppLevelCount from AndroidApplication where AppId=%d';
  SQL_SET_LEVEL = 'update AndroidApplication set AppLevel=%f, AppLevelCount=AppLevelCount+1 where AppId=%d';
  SQL_SET_DOWN_COUNT = 'update AndroidApplication set AppDownCount=AppDownCount+1 where AppId=%d';
  SQL_SEARCH_APP_BASE = 'select top %d * from AndroidApplication where 1=1';
  SQL_ADD_CATEGORY = ' and (AppCategoryCn=''%s'' or AppCategoryEn=''%s'')';
  SQL_ADD_NAME = 'and AppName like ''%%%s%%''';
  SQL_SEARCH_ORDER = ' order by AppAddDate desc';

  APP_DBTYPE = 'DBType';
  APP_SQL = 'MSSQL';
  APP_ACCESS = 'ACCESS';
  APP_DBHOST = 'DBHost';
  APP_DBNAME = 'DBName';
  APP_DBUSER = 'DBUser';
  APP_DBPWD = 'DBPwd';
  APP_DBFILE = 'DBFile';

  FIELD_LEVEL_COUNT = 'LvCnt';
  FIELD_APP_LEVEL_COUNT = 'AppLevelCount';
  FIELD_APP_ID = 'AppId';
  FIELD_APP_NAME = 'AppName';
  FIELD_APP_ICON = 'AppIcon';
  FIELD_APP_DESC = 'AppDesc';
  FIELD_APP_SIZE = 'AppSize';
  FIELD_APP_LEVEL = 'AppLevel';
  FIELD_APP_AUTHOR = 'AppAuthor';
  FIELD_APP_WEBSITE = 'AppWebsite';
  FIELD_APP_MAIL = 'AppMail';
  FIELD_APP_CATEGORY_EN = 'AppCategoryEn';
  FIELD_APP_CATEGORY_CN = 'AppCategoryCn';
  FIELD_APP_DOWN_COUNT = 'AppDownCount';
  FIELD_APP_PLATFORM = 'AppPlatform';
  FIELD_APP_ADD_DATE = 'AppAddDate';
  FIELD_APP_FILE = 'AppFile';
  FIELD_APP_VERSION = 'AppVersion';

implementation

end.
