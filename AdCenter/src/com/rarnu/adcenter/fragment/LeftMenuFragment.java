package com.rarnu.adcenter.fragment;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;

import com.rarnu.adcenter.R;
import com.rarnu.adcenter.adapter.LeftMenuItemAdapter;
import com.rarnu.adcenter.intf.Intf;
import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

public class LeftMenuFragment extends BaseFragment implements
		OnItemClickListener {

	ListView lvArea;
	ListView lvUser;
	List<String> listArea;
	List<String> listUser;
	LeftMenuItemAdapter adapterArea;
	LeftMenuItemAdapter adapterUser;
	
	public LeftMenuFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_left_menu);
	}

	@Override
	public int getBarTitle() {
		return 0;
	}

	@Override
	public int getBarTitleWithPath() {
		return 0;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		lvArea = (ListView) innerView.findViewById(R.id.lvArea);
		lvUser = (ListView) innerView.findViewById(R.id.lvUser);
		listArea = new ArrayList<String>();
		listUser = new ArrayList<String>();

		String[] strArea = getResources().getStringArray(
				R.array.left_menu_items);
		for (String area : strArea) {
			listArea.add(area);
		}
		String[] strUser = getResources().getStringArray(
				R.array.left_menu_options);
		for (String user : strUser) {
			listUser.add(user);
		}

		adapterArea = new LeftMenuItemAdapter(getActivity(), listArea);
		adapterUser = new LeftMenuItemAdapter(getActivity(), listUser);
		lvArea.setAdapter(adapterArea);
		lvUser.setAdapter(adapterUser);

		UIUtils.makeListViewFullSize(lvArea, UIUtils.dipToPx(56));
		UIUtils.makeListViewFullSize(lvUser, UIUtils.dipToPx(56));

		lvArea.setSelector(R.drawable.list_selector);
		lvUser.setSelector(R.drawable.list_selector);
	}

	@Override
	public void initEvents() {
		lvArea.setOnItemClickListener(this);
		lvUser.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_left_menu;
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
		case R.id.lvArea:
			((Intf) getActivity()).switchPage(-1, position);
			break;
		case R.id.lvUser:
			((Intf) getActivity()).switchPage(position, -1);
			break;
		}

	}

}
