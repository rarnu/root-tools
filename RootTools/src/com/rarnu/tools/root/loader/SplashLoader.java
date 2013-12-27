package com.rarnu.tools.root.loader;

import android.content.Context;
import com.rarnu.devlib.base.BaseClassLoader;
import com.rarnu.tools.root.api.MobileApi;

public class SplashLoader extends BaseClassLoader<String> {
    public SplashLoader(Context context) {
        super(context);
    }

    @Override
    public String loadInBackground() {
        return MobileApi.getSplashImage();
    }
}
