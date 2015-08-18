package com.rarnu.tools.root.utils;

import android.content.res.AssetManager;

import java.lang.reflect.Method;

/**
 * Created by rarnu on 15-8-18.
 */
public class AssetManagerUtils {

    private AssetManager manager;

    public AssetManagerUtils(AssetManager am) {
        manager = am;
    }

    public int addAssetPath(String path) {
        int mReturn = 0;
        try {
            Method m = manager.getClass().getDeclaredMethod("addAssetPath", String.class);
            m.setAccessible(true);
            mReturn = (Integer) m.invoke(manager, path);
        } catch (Exception e) {

        }
        return mReturn;
    }

}
