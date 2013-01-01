package com.rarnu.tools.root.base;

import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;

import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

public abstract class InnerFragment extends Fragment {

protected View innerView;
	
	@Override
	public void onAttach(Activity activity) {
		super.onAttach(activity);
		if (GlobalInstance.dualPane) {
			getActivity().getActionBar().setTitle(getBarTitleWithPath());
		} else {
			getActivity().getActionBar().setTitle(getBarTitle());
		}
	}
	
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		if (getCondition()) {
			innerView = inflater.inflate(getFragmentLayoutResId(), container, false);
			initComponents();
		}
		return innerView;
	}
	
	protected abstract boolean getCondition();
	protected abstract int getBarTitle();
	protected abstract int getBarTitleWithPath();
	protected abstract void initComponents();
	protected abstract int getFragmentLayoutResId();
	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setHasOptionsMenu(true);
	}
	
	@Override
	public void onPause() {
		getFragmentManager().beginTransaction().remove(this).commit();
		getActivity().setTitle(R.string.app_name);
		super.onPause();
	}
	
}
