package com.yugioh.android.loader;

import android.content.Context;
import android.database.Cursor;
import com.rarnu.devlib.base.BaseClassLoader;
import com.yugioh.android.database.YugiohUtils;

public class AutoNameLoader extends BaseClassLoader<Cursor> {

    private String name = "";

    public AutoNameLoader(Context context) {
        super(context);
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public Cursor loadInBackground() {
        Cursor c = null;
        if (!name.equals("")) {
            c = YugiohUtils.getCardNames(getContext(), name);
        }
        return c;
    }
}
