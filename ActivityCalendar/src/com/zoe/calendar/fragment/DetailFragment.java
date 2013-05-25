package com.zoe.calendar.fragment;

import android.content.ComponentName;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.TextView;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseFragment;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.classes.GoogleCalendar;
import com.zoe.calendar.common.MenuIds;
import com.zoe.calendar.utils.GoogleCalendarUtils;

public class DetailFragment extends BaseFragment {

	TextView tvTitle;
	TextView tvLocation;
	TextView tvContent;
	ActivityItem item;
	GoogleCalendar gc;

	MenuItem miShare, miUrl, miMap, miCalendar;

	public DetailFragment(String tag) {
		super(tag, "");
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
		tvTitle = (TextView) innerView.findViewById(R.id.tvTitle);
		tvLocation = (TextView) innerView.findViewById(R.id.tvLocation);
		tvContent = (TextView) innerView.findViewById(R.id.tvContent);
		gc = GoogleCalendarUtils.getCalendars(getActivity());
	}

	@Override
	public void initEvents() {

	}

	@Override
	public void initLogic() {
		item = (ActivityItem) getActivity().getIntent().getSerializableExtra(
				"item");
		tvTitle.setText(item.title);
		tvLocation.setText(item.location);
		tvContent.setText(item.content);
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_detail;
	}

	@Override
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initMenu(Menu menu) {
		miMap = menu.add(0, MenuIds.MENU_MAP, 21, R.string.menu_map);
		miMap.setIcon(R.drawable.abi_map);
		miMap.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		miUrl = menu.add(0, MenuIds.MENU_URL, 22, R.string.menu_url);
		miUrl.setIcon(R.drawable.abi_url);
		miUrl.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		miCalendar = menu.add(0, MenuIds.MENU_CALENDAR, 23,
				R.string.menu_calendar);
		miCalendar.setIcon(R.drawable.abi_calendar);
		miCalendar.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		miShare = menu.add(0, MenuIds.MENU_SHARE, 24, R.string.menu_share);
		miShare.setIcon(R.drawable.abi_share);
		miShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_MAP:
			break;
		case MenuIds.MENU_URL:
			startWebBrowser();
			break;
		case MenuIds.MENU_CALENDAR:
			if (gc == null) {
				Toast.makeText(getActivity(), R.string.toast_no_google_account,
						Toast.LENGTH_LONG).show();
				return true;
			}
			if (GoogleCalendarUtils.eventExists(getActivity(), gc, this.item)) {
				GoogleCalendarUtils.deleteEvent(getActivity(), gc, this.item);
			} else {
				GoogleCalendarUtils.addEvent(getActivity(), gc, this.item);
			}
			startCalendar();
			break;
		case MenuIds.MENU_SHARE:
			break;
		}
		return super.onOptionsItemSelected(item);
	}
	
	private void startWebBrowser() {
		try {
			Intent intent = new Intent(Intent.ACTION_VIEW);
			intent.setData(Uri.parse(item.url));
			startActivity(intent);
		} catch (Exception e) {
			
		}
	}

	private void startCalendar() {
		try {
			Intent intent = new Intent();

			intent.setComponent(new ComponentName("com.android.calendar",
					"com.android.calendar.LaunchActivity"));
			startActivity(intent);
		} catch (Exception e) {
			Toast.makeText(getActivity(), R.string.toast_google_calendar,
					Toast.LENGTH_LONG).show();
		}
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
