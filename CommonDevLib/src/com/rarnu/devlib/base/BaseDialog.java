package com.rarnu.devlib.base;

import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import com.rarnu.devlib.base.intf.InnerIntf;

public abstract class BaseDialog extends Activity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getCondition()) {
            finish();
            return;
        }
        replace();
    }

    public void replace() {
        Fragment bf = replaceFragment();
        getFragmentManager().beginTransaction().replace(android.R.id.content, bf, ((InnerIntf) bf).getTagText()).commit();
    }

    public abstract boolean getCondition();

    public abstract Fragment replaceFragment();

}
