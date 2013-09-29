package com.yugioh.android.loader;

import android.content.Context;
import android.database.Cursor;
import com.rarnu.devlib.base.BaseClassLoader;
import com.yugioh.android.database.FavUtils;
import com.yugioh.android.database.YugiohUtils;

public class FavLoader extends BaseClassLoader<Cursor> {
    public FavLoader(Context context) {
        super(context);
    }

    @Override
    public Cursor loadInBackground() {
        Cursor cResult = null;
        Cursor c = FavUtils.queryAllFav(getContext());
        if (c != null) {
            int[] ids = new int[c.getCount()];
            c.moveToFirst();
            int idx = 0;
            while (!c.isAfterLast()) {
                ids[idx] = c.getInt(c.getColumnIndex("cardId"));
                idx++;
                c.moveToNext();
            }
            c.close();
            if (ids != null && ids.length != 0) {
                cResult = YugiohUtils.getCardsViaIds(getContext(), ids);
            }

        }
        return cResult;
    }
}
