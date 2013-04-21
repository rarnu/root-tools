package com.yugioh.android.fragments;

import java.util.ArrayList;
import java.util.List;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import com.rarnu.devlib.base.BaseFragment;
import com.yugioh.android.MainActivity;
import com.yugioh.android.R;

public class RightMenuFragment extends BaseFragment implements
		OnItemClickListener {

	ListView lvAbout;
	List<String> listAbout;
	ArrayAdapter<String> adapterAbout;

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
		lvAbout = (ListView) innerView.findViewById(R.id.lvAbout);
		listAbout = new ArrayList<String>();
		listAbout.add(getString(R.string.rm_about));
		adapterAbout = new ArrayAdapter<String>(getActivity(),
				R.layout.item_menu, listAbout);
		lvAbout.setAdapter(adapterAbout);
	}

	@Override
	protected void initEvents() {
		lvAbout.setOnItemClickListener(this);
	}

	@Override
	protected void initLogic() {

	}

	@Override
	protected int getFragmentLayoutResId() {
		return R.layout.menu_right;
	}

	@Override
	protected String getMainActivityName() {
		return MainActivity.class.getName();
	}

	@Override
	protected void initMenu(Menu menu) {

	}

	@Override
	protected void onGetNewArguments(Bundle bn) {

	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		switch (parent.getId()) {
		case R.id.lvAbout:
			// TODO: about
			break;
		}

	}

	@Override
	protected String getCustomTitle() {
		return null;
	}

}
