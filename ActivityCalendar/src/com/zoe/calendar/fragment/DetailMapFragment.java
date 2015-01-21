package com.zoe.calendar.fragment;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.Menu;

import com.baidu.mapapi.map.MapController;
import com.baidu.mapapi.map.MapView;
import com.baidu.platform.comapi.basestruct.GeoPoint;
import com.rarnu.devlib.base.BaseFragment;
import com.zoe.calendar.Global;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.common.Actions;
import com.zoe.calendar.location.OverItemT;
import com.zoe.calendar.utils.ResourceUtils;

public class DetailMapFragment extends BaseFragment {

	ActivityItem actItem;
	MapView mvBaidu;
	MapController mMapController;

	public DetailMapFragment() {
		super();
		// tagText = ResourceUtils.getString(R.tag.fragment_detail_map);
		tabTitle = ResourceUtils.getString(R.string.menu_map);
	}
	
	public DetailMapFragment(String tag, String title) {
		super(tag, title);
	}

	@Override
	public int getBarTitle() {
		return R.string.detail_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.detail_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {
		mvBaidu = (MapView) innerView.findViewById(R.id.mvBaidu);
		mMapController = mvBaidu.getController();
		GeoPoint centerpt = mvBaidu.getMapCenter();
		mMapController.enableClick(true);
		mMapController.setZoom(mvBaidu.getMaxZoomLevel() - 1);
		mMapController.setCenter(centerpt);
		mvBaidu.setDoubleClickZooming(true);
	}

	@Override
	public void initEvents() {
		mvBaidu.setOnTouchListener(null);
	}

	@Override
	public void initLogic() {
		actItem = (ActivityItem) getActivity().getIntent()
				.getSerializableExtra("item");
		Global.locProvider.searchAddress(Global.city, actItem.location);
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_detail_map;
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

	class GeoPoiReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {

			GeoPoint point = new GeoPoint(intent.getIntExtra("lat", 0),
					intent.getIntExtra("lng", 0));

			mMapController.animateTo(point);

			Drawable marker = getResources().getDrawable(R.drawable.icon_mark);
			marker.setBounds(0, 0, marker.getIntrinsicWidth(),
					marker.getIntrinsicHeight());
			mvBaidu.getOverlays().clear();
			mvBaidu.getOverlays().add(
					new OverItemT(marker, getActivity(), point, intent
							.getStringExtra("addr"), mvBaidu));
			mvBaidu.refresh();

		}

	}

	private GeoPoiReceiver receiverGeoPoi = new GeoPoiReceiver();
	private IntentFilter filterGeoPoi = new IntentFilter(
			Actions.ACTION_RECEIVE_GEOPOI);

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		getActivity().registerReceiver(receiverGeoPoi, filterGeoPoi);
	}

	@Override
	public void onDestroy() {
		getActivity().unregisterReceiver(receiverGeoPoi);
		super.onDestroy();
	}
}
