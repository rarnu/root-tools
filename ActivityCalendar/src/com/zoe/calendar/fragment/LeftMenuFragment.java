package com.zoe.calendar.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.common.ISliding;
import com.zoe.calendar.CityActivity;
import com.zoe.calendar.R;
import com.zoe.calendar.SettingsActivity;
import com.zoe.calendar.adapter.LeftMenuAdapter;
import com.zoe.calendar.classes.LeftMenuItem;

public class LeftMenuFragment extends BaseFragment implements
		OnItemClickListener {

	ListView lvLeftCard;
	LeftMenuAdapter adapter;
	List<LeftMenuItem> list;

	public LeftMenuFragment(String tag) {
		super(tag, "");
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
		lvLeftCard = (ListView) innerView.findViewById(R.id.lvLeftCard);

		String[] titles = getResources()
				.getStringArray(R.array.title_left_menu);
		list = new ArrayList<LeftMenuItem>();

		for (int i = 0; i < titles.length; i++) {
			list.add(new LeftMenuItem(titles[i], getResources().getIdentifier(
					String.format("left_icon_%d", i + 1), "drawable",
					getActivity().getPackageName())));
		}

		adapter = new LeftMenuAdapter(getActivity(), list);
		lvLeftCard.setAdapter(adapter);

	}

	@Override
	public void initEvents() {
		lvLeftCard.setOnItemClickListener(this);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_leftmenu;
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
	public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
		switch (position) {
		case 0:
			break;
		case 1:
			startActivity(new Intent(getActivity(), CityActivity.class));
			break;
		case 2:
			startActivity(new Intent(getActivity(), SettingsActivity.class));
			break;
		}

		((ISliding) getActivity()).toggle();

	}

}
