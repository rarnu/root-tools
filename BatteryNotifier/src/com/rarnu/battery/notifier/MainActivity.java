package com.rarnu.battery.notifier;

import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.ListView;

import com.rarnu.battery.notifier.adapter.BattaryAdapter;
import com.rarnu.battery.notifier.classes.BatteryState;
import com.rarnu.battery.notifier.common.MenuIds;
import com.rarnu.battery.notifier.service.BatteryService;
import com.rarnu.utils.ResourceUtils;
import com.rarnu.utils.UIUtils;

public class MainActivity extends Activity implements Observer {

	ListView lvBattery;
	BattaryAdapter adapter;
	List<BatteryState> list;
	MenuItem miSettings;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(this, getWindowManager(), false);
		ResourceUtils.init(this);
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		lvBattery = (ListView) findViewById(R.id.lvBattery);
		Global.keeper.addObserver(this);
		list = new ArrayList<BatteryState>();
		adapter = new BattaryAdapter(this, list);
		lvBattery.setAdapter(adapter);

		startService(new Intent(this, BatteryService.class));
	}

	@Override
	protected void onDestroy() {
		Global.keeper.deleteObserver(this);
		super.onDestroy();
	}

	@Override
	public void update(Observable observable, Object data) {
		list = Global.keeper.getKeepedList();
		adapter.setNewList(list);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		menu.clear();
		miSettings = menu.add(0, MenuIds.MENUID_SETTINGS, 99,
				R.string.menu_settings);
		miSettings.setIcon(android.R.drawable.ic_menu_preferences);
		miSettings.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);
		return super.onCreateOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENUID_SETTINGS:
			startActivity(new Intent(this, SettingsActivity.class));
			break;
		}
		return super.onOptionsItemSelected(item);
	}

}
