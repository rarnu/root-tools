import os
import os.path
import sqlite3
import shutil
import types

import yugioh_env


class yugioh:
    __DATABASE_NAME = "yugioh.db"
    __TBL_DATA = "YGODATA"

    def __init__(self):
        pass

    def __get_version(self):
        conn = sqlite3.connect(self.__DATABASE_NAME)
        c = conn.cursor()
        c.execute("SELECT ver FROM version")
        row = c.fetchone()
        ver = row[0]
        c.close()
        conn.close()
        return ver

    def __set_version(self, conn, c, ver):
        c.execute("INSERT INTO version (ver) VALUES (%d)" % (int(ver)))
        conn.commit()

    def __import_from_sqlite(self, conn, c, ori_db):
        conn_ori = sqlite3.connect(ori_db)
        c_ori = conn_ori.cursor()
        c_ori.execute("SELECT * FROM ygodata")
        rows_ori = c_ori.fetchall()

        count = 0
        param_str = "?"
        param_count = 25

        for i in range(0, param_count - 1, 1):
            param_str += ",?"
        sql_fmt = "INSERT INTO YGODATA VALUES (%s)" % (param_str)
        for row in rows_ori:
            for i in range(0, len(row), 1):
                if type(row[i]) is types.StringType:
                    row[i] = row[i].decode("utf-8")
            list_param = list(row)
            list_param.insert(0, row[0])

            c.execute(sql_fmt, list_param)
            count += 1
        conn.commit()
        c_ori.close()
        conn_ori.close()
        return count

    def __create_database(self):
        if os.path.exists(self.__DATABASE_NAME):
            os.remove(self.__DATABASE_NAME)
        conn = sqlite3.connect(self.__DATABASE_NAME)
        return conn


    def __generate_android_data(self, conn, c):
        c.execute("CREATE TABLE android_metadata(locale text)")
        c.execute("INSERT INTO android_metadata VALUES ('en_US')")
        conn.commit()

    def __generate_version_data(self, conn, c):
        c.execute("CREATE TABLE version(ver int primary key)")
        conn.commit()

    def __generate_tables(self, conn, c):
        c.execute(
            '''CREATE TABLE YGODATA (
                _id int PRIMARY KEY,
                japName text DEFAULT 'NULL',
                name text DEFAULT 'NULL',
                enName text DEFAULT 'NULL',
                sCardType text DEFAULT 'NULL',
                CardDType text DEFAULT 'NULL',
                tribe text DEFAULT 'NULL',
                package text DEFAULT 'NULL',
                element text DEFAULT 'NULL',
                level int DEFAULT 'NULL',
                infrequence text DEFAULT 'NULL',
                atkValue int DEFAULT 'NULL',
                atk text DEFAULT 'NULL',
                defValue int DEFAULT 'NULL',
                def text DEFAULT 'NULL',
                effect text,
                ban text DEFAULT 'NULL',
                cheatcode text DEFAULT 'NULL',
                adjust text,
                cardCamp text,
                oldName text,
                shortName text,
                pendulumL int,
                pendulumR int
                )''')
        conn.commit()

    def convert_mdb(self, ori_db, ver, output):
        print("create database")
        conn = self.__create_database()
        c = conn.cursor()
        print("generate table and fill basic data")
        self.__generate_android_data(conn, c)
        self.__generate_version_data(conn, c)
        self.__generate_tables(conn, c)
        self.__set_version(conn, c, ver)
        print("import card data")
        count_data = self.__import_from_sqlite(conn, c, ori_db)
        c.close()
        conn.close()
        print("output database as file")
        if os.path.isdir(output):
            output += self.__DATABASE_NAME
        if os.path.exists(output):
            os.remove(output)
        shutil.copyfile(self.__DATABASE_NAME, output)
        print("import completed, card:%d" % (count_data))

    def __change_id(self, conn, c):
        c.execute("alter table YGODATA RENAME TO YGODATA_OLD")
        self.__generate_tables(conn, c)
        c.execute("INSERT INTO YGODATA SELECT * FROM YGODATA_OLD")
        c.execute("DROP TABLE YGODATA_OLD")
        conn.commit()
        pass

    def convert_db(self, ori_db, ver, output):
        print("copy database")
        db_path = os.path.join(output, self.__DATABASE_NAME)
        shutil.copyfile(ori_db, db_path)
        conn = sqlite3.connect(db_path)
        c = conn.cursor()
        self.__generate_android_data(conn, c)
        self.__generate_version_data(conn, c)
        self.__set_version(conn, c, ver)
        self.__change_id(conn, c)
        print("convert data completed")

    def get_version(self):
        if os.path.exists(self.__DATABASE_NAME):
            os.remove(self.__DATABASE_NAME)
        yugioh_env.execmd("adb pull /sdcard/.yugioh/yugioh.db ./")
        if os.path.exists(self.__DATABASE_NAME):
            print("current version: %s" % (self.__get_version()))

