package com.rarnu.devlib.base.inner;

import android.app.Fragment;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;

import com.rarnu.devlib.common.UIInstance;

public abstract class InnerFragment extends Fragment implements
		OnGlobalLayoutListener {

	protected View innerView = null;
	protected Bundle innerBundle = null;

	protected String tagText;
	protected String tabTitle;

	public String getTagText() {
		return tagText;
	}

	public String getTabTitle() {
		return tabTitle;
	}

	public InnerFragment() {
		super();
	}

	public InnerFragment(String tagText, String tabTitle) {
		super();
		this.tagText = tagText;
		this.tabTitle = tabTitle;
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container,
			Bundle savedInstanceState) {
		Log.e("InnerFragment", "onCreateView");
		innerView = inflater
				.inflate(getFragmentLayoutResId(), container, false);
		initComponents();
		initEvents();
		innerView.getViewTreeObserver().addOnGlobalLayoutListener(this);
		return innerView;
	}

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		Log.e("InnerFragment", "onActivityCreated");
		innerBundle = getArguments();
		initLogic();
		if (getCustomTitle() == null || getCustomTitle().equals("")) {
			if (UIInstance.dualPane) {
				getActivity().getActionBar().setTitle(getBarTitleWithPath());
			} else {
				getActivity().getActionBar().setTitle(getBarTitle());
			}
		} else {
			getActivity().getActionBar().setTitle(getCustomTitle());
		}
	}

	protected abstract int getBarTitle();

	protected abstract int getBarTitleWithPath();

	protected abstract String getCustomTitle();

	protected abstract void initComponents();

	protected abstract void initEvents();

	protected abstract void initLogic();

	protected abstract int getFragmentLayoutResId();

	protected abstract String getMainActivityName();

	protected abstract void initMenu(Menu menu);

	protected abstract void onGetNewArguments(Bundle bn);

	public void setNewArguments(Bundle bn) {
		innerBundle = getArguments();
		onGetNewArguments(bn);
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		Log.e("InnerFragment", "onCreate");
		setHasOptionsMenu(true);
	}

	@Override
	public void onPause() {
		if (getCustomTitle() == null || getCustomTitle().equals("")) {
			getActivity().setTitle(getBarTitle());
		} else {
			getActivity().getActionBar().setTitle(getCustomTitle());
		}
		super.onPause();
	}

	@Override
	public void onCreateOptionsMenu(Menu menu, MenuInflater inflater) {
		if (getActivity() == null) {
			return;
		}
		if (getActivity().getClass().getName().equals(getMainActivityName())
				&& !UIInstance.dualPane) {
			return;
		}

		initMenu(menu);
	}

	@Override
	public void onGlobalLayout() {
		onLayoutReady();
	}

	protected void onLayoutReady() {

	}

}
