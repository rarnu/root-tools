package com.rarnu.devlib.common;

import android.app.Activity;
import android.app.Fragment;
import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.content.Intent;

import com.rarnu.devlib.R;

public class FragmentStarter {

	public static void showContent(Activity activity, Class<?> clz,
			Fragment fContent) {
		if (UIInstance.dualPane) {
			FragmentManager fragmentManager = activity.getFragmentManager();
			FragmentTransaction fragmentTransaction = fragmentManager
					.beginTransaction();
			fragmentTransaction.replace(R.id.fragmentDetail, fContent);
			fragmentTransaction
					.setTransition(FragmentTransaction.TRANSIT_FRAGMENT_FADE);
			fragmentTransaction.commit();
		} else {
			activity.startActivity(new Intent(activity, clz));
		}
	}
}
