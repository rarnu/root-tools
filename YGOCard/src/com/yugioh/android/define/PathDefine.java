package com.yugioh.android.define;

import android.os.Environment;

import java.io.File;

public class PathDefine {

    public static final String ROOT_PATH = Environment.getExternalStorageDirectory().getAbsolutePath() + "/.yugioh/";
    public static final String APK_NAME = "YuGiOhCard.apk";
    public static final String DATA_ZIP = "yugioh.zip";
    public static final String DATA_NAME = "yugioh.db";
    public static final String FAV_NAME = "fav.db";
    public static final String DATABASE_PATH = ROOT_PATH + DATA_NAME;
    public static final String FAV_DATABASE_NAME = ROOT_PATH + FAV_NAME;
    public static final String PICTURE_PATH = ROOT_PATH + "images/";
    public static final String DOWNLOAD_PATH = ROOT_PATH + "downloads/";
    public static final String RECOMMAND_PATH = ROOT_PATH + "recommand/";
    public static final String PACK_PATH = ROOT_PATH + "pack/";
    public static final String DECK_PATH = ROOT_PATH + "deck/";
    public static final String PACK_LIST = PACK_PATH + "list";
    public static final String PACK_ITEM = PACK_PATH + "pack_%s";
    public static final String DECK_LIST = DECK_PATH + "list";
    public static final String DECK_ITEM = DECK_PATH + "deck_%s";

    public static void init() {
        mkdir(ROOT_PATH);
        mkdir(PICTURE_PATH);
        mkdir(DOWNLOAD_PATH);
        mkdir(RECOMMAND_PATH);
        mkdir(PACK_PATH);
        mkdir(DECK_PATH);
    }

    private static void mkdir(String path) {
        File fPath = new File(path);
        if (!fPath.exists()) {
            fPath.mkdirs();
        }
    }

}
