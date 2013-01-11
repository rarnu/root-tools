package com.rarnu.vim.emotion.common.base;

import android.app.Fragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

public abstract class BaseFragment extends Fragment {

	protected View innerView;
	
	public abstract int getFragmentLayout();
	public abstract void initComponents();
	public abstract void init();
	
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		innerView = inflater.inflate(getFragmentLayout(), container, false);
		initComponents();
		return innerView;
	}
	
	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		init();
	}
	
}
