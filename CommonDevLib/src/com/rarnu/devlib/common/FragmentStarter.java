package com.rarnu.devlib.common;

import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.content.Intent;
import com.rarnu.devlib.R;
import com.rarnu.devlib.base.intf.InnerIntf;

public class FragmentStarter {

    public static void showContent(Activity activity, Class<?> clz, Fragment fContent) {
        if (UIInstance.dualPane) {
            activity.getFragmentManager().beginTransaction().replace(R.id.fragmentDetail, fContent, ((InnerIntf) fContent).getTagText()).setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE).commit();
        } else {
            activity.startActivity(new Intent(activity, clz));
        }
    }
}
