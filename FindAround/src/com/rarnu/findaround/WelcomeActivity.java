package com.rarnu.findaround;

import java.util.ArrayList;
import java.util.List;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.location.Location;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.Gallery;
import android.widget.TextView;
import android.widget.Toast;

import com.baidu.mapapi.GeoPoint;
import com.baidu.mapapi.LocationListener;
import com.baidu.mapapi.MKLocationManager;
import com.rarnu.findaround.adapter.WelcomeAdapter;
import com.rarnu.findaround.api.BaiduAPI;
import com.rarnu.findaround.base.BaseActivity;
import com.rarnu.findaround.common.PageItem;
import com.rarnu.findaround.common.PageUtils;
import com.rarnu.findaround.common.UIUtils;
import com.rarnu.findaround.comp.GridPage;

public class WelcomeActivity extends BaseActivity implements OnClickListener,
		LocationListener {

	Gallery gButtons;
	TextView tvAddress, tvGeo;

	Button btnRight;

	WelcomeAdapter welcomeAdapter = null;
	List<GridPage> listWelcome = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		UIUtils.initDisplayMetrics(getWindowManager());
		setContentView(R.layout.welcome);
		init();

		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().start();

	}

	@Override
	protected void onDestroy() {
		GlobalInstance.point = null;
		super.onDestroy();
	}

	@Override
	protected void onResume() {

		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_NETWORK_PROVIDER);
		app.getMapManager().getLocationManager()
				.enableProvider(MKLocationManager.MK_GPS_PROVIDER);
		app.getMapManager().getLocationManager().requestLocationUpdates(this);

		app.getMapManager().start();

		super.onResume();
		registerReceiver(myreceiver, mapFilter);
	}

	@Override
	protected void onPause() {
		MainApplication app = (MainApplication) getApplication();
		app.getMapManager().getLocationManager().removeUpdates(this);
		app.getMapManager().getLocationManager()
				.disableProvider(MKLocationManager.MK_NETWORK_PROVIDER);
		app.getMapManager().getLocationManager()
				.disableProvider(MKLocationManager.MK_GPS_PROVIDER);

		app.getMapManager().stop();

		unregisterReceiver(myreceiver);

		super.onPause();
	}

	@Override
	protected void mappingComponents() {
		super.mappingComponents();
		gButtons = (Gallery) findViewById(R.id.gButtons);
		tvAddress = (TextView) findViewById(R.id.tvAddress);
		tvGeo = (TextView) findViewById(R.id.tvGeo);
		btnRight = (Button) findViewById(R.id.btnRight);
	}

	@Override
	protected void init() {

		super.init();
		gButtons.setSpacing(getSpacing());
		initGrid9();
		btnRight.setText(R.string.settings);
		btnRight.setVisibility(View.VISIBLE);
		btnRight.setOnClickListener(this);
	}

	private void initGrid9() {

		listWelcome = new ArrayList<GridPage>();

		List<PageItem[]> pages = PageUtils.buildPages(this);
		for (int i = 0; i < pages.size(); i++) {
			GridPage gp = new GridPage(this);
			gp.setGallery(gButtons);
			gp.setButtonsItem(pages.get(i));
			gp.setButtonClickEvent(this);
			listWelcome.add(gp);
		}

		welcomeAdapter = new WelcomeAdapter(listWelcome);
		gButtons.setAdapter(welcomeAdapter);
	}

	private int getSpacing() {
		int gridWidth = UIUtils.dipToPx(300);
		int screenWidth = getWindowManager().getDefaultDisplay().getWidth();
		return screenWidth - gridWidth;
	}

	@Override
	public void onClick(View v) {

		if (v.getId() == R.id.btnRight) {
			Intent inSettings = new Intent(this, SettingsActivity.class);
			startActivityForResult(inSettings, 0);
		} else {

			if (GlobalInstance.point == null) {
				Toast.makeText(this, R.string.no_location_found,
						Toast.LENGTH_LONG).show();
				return;
			}
			Intent inList = new Intent(this, PoiListActivity.class);
			inList.putExtra("keyword", ((TextView) v).getText().toString());
			startActivity(inList);
		}
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		if (requestCode == 0) {
			initGrid9();
		}
	}
	
	@Override
	public void onLocationChanged(Location location) {
		if (location == null) {
			GlobalInstance.point = null;
		} else {
			GlobalInstance.point = new GeoPoint(
					(int) (location.getLatitude() * 1e6),
					(int) (location.getLongitude() * 1e6));
			tvAddress.setText(R.string.addressing);
			tvGeo.setText(String.format("%f, %f", location.getLatitude(),
					location.getLongitude()));
			getAddressByGeo(location.getLatitude(), location.getLongitude());

		}
	}

	private void getAddressByGeo(final double lat, final double lng) {

		final Handler h = new Handler() {
			@Override
			public void handleMessage(Message msg) {
				if (msg.what == 1) {
					tvAddress.setText(GlobalInstance.address);
					if (GlobalInstance.address.equals("")) {
						tvAddress.setText(R.string.cannot_get_address);
					}
				}
				super.handleMessage(msg);
			}
		};

		new Thread(new Runnable() {

			@Override
			public void run() {
				String addr = BaiduAPI.getAddressViaGeo(lat, lng);
				GlobalInstance.address = addr;
				h.sendEmptyMessage(1);

			}
		}).start();

	}

	class MapReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			tvAddress.setText(R.string.network_error);
			Toast.makeText(context, R.string.network_error, Toast.LENGTH_LONG)
					.show();
		}
	}

	private MapReceiver myreceiver = new MapReceiver();
	private IntentFilter mapFilter = new IntentFilter(
			MainApplication.NETWORK_ERROR_ACTION);
}
