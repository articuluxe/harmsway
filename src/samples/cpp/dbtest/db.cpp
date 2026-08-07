// db.cpp --- freetds testing
// Author: Dan Harms <dan.harms@xrtrading.com>
// Created: Thursday, July  2, 2015
// Modified Time-stamp: <2019-07-16 10:26:48 dan.harms>
// Modified by: Dan Harms
// run as TDSVER=7.3 LD_LIBRARY_PATH=/usr/local/freetds/0.95.95/lib64 db

#include <sybfront.h>
#include <sybdb.h>

#include <iostream>
#include <sstream>

int msg_handler(DBPROCESS* proc, DBINT msgno, int msgstate,
    int severity, char* msg, char* server, char* process,
    int line)
{
    std::cout << "[tds][" << severity << "] "
              << msg
              << " (" << msgno << ") "
              << proc << "::" << line
              << std::endl;
    return 0;
}

int err_handler(DBPROCESS* proc, int severity, int dberr,
    int oserr, char* dbStr, char* osStr)
{
    std::cerr << "[tds] ";
    if (dberr)
        std::cerr << "DB error: " << dbStr << " ("
                  << dberr
                  << ")"
                  << std::endl;
    if (oserr)
        std::cerr << "OS error: " << osStr
                  << " (" << oserr << ")"
                  << std::endl;
    return INT_CANCEL;
}

void exec(DBPROCESS* proc)
{
    RETCODE erc = dbsqlexec(proc);
    while ((erc = dbresults(proc)) != NO_MORE_RESULTS)
    {
        dbprhead(proc);
        while ((erc = dbnextrow(proc)) != NO_MORE_ROWS)
        {
            dbprrow(proc);
        }
    }
}

int main(int argc, char* argv[])
{
    if (dbinit() == FAIL)
    {
        std::cerr << "[ERR] dbinit" << std::endl;
        return 1;
    }
    dberrhandle(err_handler);
    dbmsghandle(msg_handler);
    LOGINREC* login = dblogin();
    if (dbsetversion(DBVERSION_73) == FAIL)
    {
        std::cerr << "[ERR] dbsetversion" << std::endl;
        return 1;
    }
    DBSETLUSER(login, "Xmock");
    DBSETLPWD(login, "XMockpword");

    DBPROCESS* proc = dbopen(login, "CHIHQ-SQLPRD-2V");
    RETCODE erc = dbuse(proc, "InstrumentTablesBeta");
    if (erc != SUCCEED)
    {
        std::cerr << "[ERR] dbuse" << std::endl;
        return 1;
    }

    const char* select = "SELECT * FROM trader.vw_SymbolReference "
        "WHERE Exchange=\'CME\' AND BeginDate<=\'2016-05-10\' "
        "AND (EndDate is null OR EndDate>=\'2016-05-10\') "
        "AND NormalizedSymbol=\'ESM16\' "
        ;
    if (dbcmd(proc, select) == FAIL)
    {
        std::cerr << "[ERR] SELECT " << std::endl;
        return 1;
    }
    exec(proc);

    DBDATETIME value;
    if (dbbind(proc, 2, DATETIMEBIND, sizeof(value),
            reinterpret_cast<BYTE*>(&value)) == FAIL)
    {
        std::cerr << "[ERR] dbbind" << std::endl;
        return 1;
    }

    return 0;
}

// code ends here
