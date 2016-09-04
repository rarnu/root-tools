package com.rarnu.tools.neo.base;

import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;

public abstract class BaseDialog extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        if (customTheme() != 0) {
            setTheme(customTheme());
        }
        super.onCreate(savedInstanceState);
        if (getCondition()) {
            finish();
            return;
        }
        replace();
    }

    public void replace() {
        Fragment bf = replaceFragment();
        getFragmentManager().beginTransaction().replace(android.R.id.content, bf).commit();
    }

    public abstract boolean getCondition();

    public abstract Fragment replaceFragment();

    public abstract int customTheme();
}
