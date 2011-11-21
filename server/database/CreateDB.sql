create database AndroidAppStore
go

use AndroidAppStore
go

create table AndroidApplication (
	AppId int primary key,
	AppName varchar(50) not null,
	AppIcon varchar(255) not null,
	AppDesc varchar(1000),
	AppSize varchar(32),
	AppLevel float,
	AppLevelCount int,
	AppAuthor varchar(64),
	AppWebsite varchar(255),
	AppMail varchar(255),
	AppCategory varchar(16),
	AppDownCount int,
	AppPlatform varchar(255),
	AppAddDate datetime,
	AppFile varchar(255)
)
go