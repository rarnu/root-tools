package com.sbbs.me.android.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.AboutActivity;
import com.sbbs.me.android.IMainIntf;
import com.sbbs.me.android.R;

public class LeftMenuFragment extends BaseFragment implements
		OnItemClickListener {

	ListView lvMenu, lvExit;
	TextView tvLeftTitle;
	ImageView ivLogo;

	List<String> listMenu, listExit;
	ArrayAdapter<String> adapterMenu, adapterExit;

	public LeftMenuFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_menu_left);
	}

	@Override
	public int getBarTitle() {
		return R.string.app_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.app_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		lvMenu = (ListView) innerView.findViewById(R.id.lvMenu);
		lvExit = (ListView) innerView.findViewById(R.id.lvExit);
		tvLeftTitle = (TextView) innerView.findViewById(R.id.tvLeftTitle);
		ivLogo = (ImageView) innerView.findViewById(R.id.ivLogo);

		listMenu = new ArrayList<String>();
		listMenu.add(getString(R.string.lm_home));
		listMenu.add(getString(R.string.lm_postnew));
		listMenu.add(getString(R.string.lm_recent));
		listMenu.add(getString(R.string.lm_hottags));
		listMenu.add(getString(R.string.lm_ongithub));
		listMenu.add(getString(R.string.lm_archievement));
		listExit = new ArrayList<String>();
		listExit.add(getString(R.string.lm_about));
		listExit.add(getString(R.string.lm_exit));
		adapterMenu = new ArrayAdapter<String>(getActivity(),
				R.layout.item_menu, listMenu);
		adapterExit = new ArrayAdapter<String>(getActivity(),
				R.layout.item_menu, listExit);
		lvMenu.setAdapter(adapterMenu);
		lvExit.setAdapter(adapterExit);

		lvMenu.setSelector(R.drawable.menu_list_selector);
		lvExit.setSelector(R.drawable.menu_list_selector);
	}

	@Override
	public void initEvents() {
		lvMenu.setOnItemClickListener(this);
		lvExit.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.menu_left;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {

	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		switch (parent.getId()) {
		case R.id.lvMenu:
			((IMainIntf) getActivity()).switchPage(position, true);
			break;
		case R.id.lvExit: {
			switch (position) {
			case 0:
				startActivity(new Intent(getActivity(), AboutActivity.class));
				break;
			case 1:
				getActivity().finish();
				break;
			}

		}
			break;
		}
	}

}
