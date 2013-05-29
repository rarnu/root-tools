package com.zoe.calendar.fragment;

import java.util.ArrayList;
import java.util.List;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.zoe.calendar.Global;
import com.zoe.calendar.R;
import com.zoe.calendar.adapter.CityAdapter;
import com.zoe.calendar.classes.CityItem;
import com.zoe.calendar.common.Actions;
import com.zoe.calendar.common.Config;
import com.zoe.calendar.utils.CityUtils;

public class CityFragment extends BaseFragment implements OnClickListener,
		OnItemClickListener {

	TextView tvCityValue;
	EditText etCityFilter;
	ListView lvCities;

	List<CityItem> listCity;
	CityAdapter adapterCity;

	public CityFragment(String tag) {
		super(tag, "");
	}

	@Override
	public int getBarTitle() {
		return R.string.city_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.city_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		tvCityValue = (TextView) innerView.findViewById(R.id.tvCityValue);
		etCityFilter = (EditText) innerView.findViewById(R.id.etCityFilter);
		lvCities = (ListView) innerView.findViewById(R.id.lvCities);
		listCity = new ArrayList<CityItem>();
		adapterCity = new CityAdapter(getActivity(), listCity);
		lvCities.setAdapter(adapterCity);
	}

	@Override
	public void initEvents() {
		tvCityValue.setOnClickListener(this);
		lvCities.setOnItemClickListener(this);
		etCityFilter.addTextChangedListener(new TextWatcher() {

			@Override
			public void onTextChanged(CharSequence s, int start, int before,
					int count) {

			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count,
					int after) {

			}

			@Override
			public void afterTextChanged(Editable s) {
				adapterCity.filter(s.toString());
			}
		});
	}

	@Override
	public void initLogic() {
		loadCityListT();
	}

	private void loadCityListT() {
		final Handler hLoadCity = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					adapterCity.setNewList(listCity);
				}
				super.handleMessage(msg);
			}
		};
		new Thread(new Runnable() {

			@Override
			public void run() {
				listCity = CityUtils.loadCity(getActivity());
				hLoadCity.sendEmptyMessage(1);
			}
		}).start();
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_city;
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

	class LoationReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			if (Global.location != null) {

				tvCityValue.setText(Global.location.getCity());
			} else {
				tvCityValue.setText(R.string.city_not_found);
			}
		}
	}

	private LoationReceiver rceiverLocation = new LoationReceiver();
	private IntentFilter filterLocation = new IntentFilter(
			Actions.ACTION_RECEIVE_LOCATION);

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		getActivity().registerReceiver(rceiverLocation, filterLocation);
	}

	@Override
	public void onDestroy() {
		getActivity().unregisterReceiver(rceiverLocation);
		super.onDestroy();
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvCityValue:
			String city = tvCityValue.getText().toString();
			if (city.equals(getString(R.string.city_locating))
					|| city.equals(getString(R.string.city_not_found))) {
				return;
			}
			CityItem ci = CityUtils.findCity(city);
			if (ci != null) {
				Global.city = ci.name;
				Global.city_pinyin = ci.pinyin.toLowerCase();
				Config.setCity(getActivity(), Global.city);
				Config.setCityPinyin(getActivity(), Global.city_pinyin);
			}
			Global.synced = false;
			getActivity().finish();
			break;
		}
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
		CityItem item = listCity.get(position);
		Global.city = item.name;
		Global.city_pinyin = item.pinyin.toLowerCase();
		Config.setCity(getActivity(), Global.city);
		Config.setCityPinyin(getActivity(), Global.city_pinyin);
		Global.synced = false;
		getActivity().finish();
	}

}
