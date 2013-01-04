package com.rarnu.tools.root.base;

import android.app.Fragment;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

public abstract class BaseDialogFragment extends Fragment {

	protected View innerView = null;

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {

		Log.e(getClass().getName(), "onCreateView");
		innerView = inflater
				.inflate(getFragmentLayoutResId(), container, false);
		initComponents();
		initLogic();

		Log.e(getClass().getName(), "innerView:" + innerView);
		return innerView;
	}

	protected abstract void initComponents();

	protected abstract void initLogic();

	protected abstract int getFragmentLayoutResId();

	
}
