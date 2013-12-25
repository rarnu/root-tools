package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseClassLoader;
import com.rarnu.tools.root.api.MobileApi;
import com.rarnu.tools.root.common.TeamInfo;

import java.util.Locale;

public class TeamLoader extends BaseClassLoader<TeamInfo> {

    public TeamLoader(Context context) {
        super(context);
    }

    @Override
    public TeamInfo loadInBackground() {
        String lng = Locale.getDefault().getLanguage().toLowerCase();
        String country = Locale.getDefault().getCountry().toLowerCase();
        int lang = 0;
        if (lng.equals("zh")) {
            if (country.equals("cn")) {
                lang = 1;
            } else if (country.equals("tw")) {
                lang = 2;
            }
        }

        return MobileApi.getTeam(lang);
    }
}
