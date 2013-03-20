package com.rarnu.kevin.medic.fragment;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.RelativeLayout;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.BlockListView;
import com.rarnu.devlib.utils.UIUtils;
import com.rarnu.kevin.medic.MainActivity;
import com.rarnu.kevin.medic.R;
import com.rarnu.kevin.medic.adapter.MenuItemAdapter;

public class LeftMenuFragment extends BaseFragment implements
		OnItemClickListener, OnClickListener {

	BlockListView lvMenu, lvMenu2;
	List<String> lstMenu, lstMenu2;
	MenuItemAdapter adapterMenu, adapterMenu2;
	RelativeLayout layHeadBar;

	@Override
	protected int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	protected int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	protected void initComponents() {
		layHeadBar = (RelativeLayout) innerView.findViewById(R.id.layHeadBar);
		lvMenu = (BlockListView) innerView.findViewById(R.id.lvMenu);
		lvMenu2 = (BlockListView) innerView.findViewById(R.id.lvMenu2);
		lstMenu = new ArrayList<String>();
		lstMenu2 = new ArrayList<String>();
		adapterMenu = new MenuItemAdapter(getActivity(), lstMenu);
		adapterMenu2 = new MenuItemAdapter(getActivity(), lstMenu2);
		lvMenu.setAdapter(adapterMenu);
		lvMenu2.setAdapter(adapterMenu2);
	}

	@Override
	protected void initEvents() {
		lvMenu.setOnItemClickListener(this);
		layHeadBar.setOnClickListener(this);
	}

	@Override
	protected void initLogic() {
		lstMenu.clear();
		lstMenu.add(getString(R.string.leftmenu_0));
		lstMenu.add(getString(R.string.leftmenu_1));
		lstMenu.add(getString(R.string.leftmenu_2));
		lstMenu.add(getString(R.string.leftmenu_3));
		lstMenu.add(getString(R.string.leftmenu_4));
		adapterMenu.setNewList(lstMenu);
		lvMenu.setItemHeight(UIUtils.dipToPx(56));
		lvMenu.resize();

		lstMenu2.clear();
		lstMenu2.add(getString(R.string.leftmenu_5));
		lstMenu2.add(getString(R.string.leftmenu_6));
		adapterMenu2.setNewList(lstMenu2);
		lvMenu2.setItemHeight(UIUtils.dipToPx(56));
		lvMenu2.resize();

	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.fragment_menu_left;
	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public void onItemClick(AdapterView<?> adapter, View v, int position,
			long id) {
		((MainActivity) getActivity()).switchPage(position + 1);
		((MainActivity) getActivity()).getSlidingMenu().toggle();
	}

	@Override
	public void onClick(View v) {
		((MainActivity) getActivity()).switchPage(0);
		((MainActivity) getActivity()).getSlidingMenu().toggle();

	}

}
