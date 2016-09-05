package com.rarnu.tools.neo.loader;

import android.content.Context;
import android.content.SharedPreferences;
import com.rarnu.tools.neo.base.BaseLoader;
import com.rarnu.tools.neo.data.BanStartInfo;
import com.rarnu.tools.neo.utils.AppUtils;
import com.rarnu.tools.neo.xposed.XpStatus;

import java.util.List;

public class BannedLoader extends BaseLoader<BanStartInfo> {

    public BannedLoader(Context context) {
        super(context);

    }

    @Override
    public List<BanStartInfo> loadInBackground() {
        SharedPreferences pref = getContext().getSharedPreferences(XpStatus.PREF, 1);
        return AppUtils.getAppsWithBannedStatue(getContext(), pref);
    }
}
