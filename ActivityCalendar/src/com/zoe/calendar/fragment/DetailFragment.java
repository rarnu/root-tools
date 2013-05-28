package com.zoe.calendar.fragment;

import java.util.List;

import android.app.Fragment;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseTabFragment;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.classes.GoogleCalendar;
import com.zoe.calendar.common.MenuIds;
import com.zoe.calendar.utils.GoogleCalendarUtils;

public class DetailFragment extends BaseTabFragment {

	MenuItem miCalendar, miShare;
	GoogleCalendar gc;
	ActivityItem actItem;

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
	public String getMainActivityName() {
		return "";
	}

	@Override
	public void initComponents() {
		super.initComponents();
		gc = GoogleCalendarUtils.getCalendars(getActivity());
	}

	@Override
	public void initMenu(Menu menu) {
		miCalendar = menu.add(0, MenuIds.MENU_CALENDAR, 23,
				R.string.menu_calendar);
		miCalendar.setIcon(android.R.drawable.ic_menu_agenda);
		miCalendar.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		miShare = menu.add(0, MenuIds.MENU_SHARE, 24, R.string.menu_share);
		miShare.setIcon(android.R.drawable.ic_menu_share);
		miShare.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case MenuIds.MENU_CALENDAR:
			if (gc == null) {
				Toast.makeText(getActivity(), R.string.toast_no_google_account,
						Toast.LENGTH_LONG).show();
				return true;
			}
			if (GoogleCalendarUtils.eventExists(getActivity(), gc, actItem)) {
				GoogleCalendarUtils.deleteEvent(getActivity(), gc, actItem);
			} else {
				GoogleCalendarUtils.addEvent(getActivity(), gc, actItem);
			}
			startCalendar();
			break;
		case MenuIds.MENU_SHARE:
			break;
		}
		return super.onOptionsItemSelected(item);
	}

	private void startCalendar() {
		// goto the pointed day directly
		// TODO: do not do this now, only shows that added or deleted with BIG
		// dialogs
		// try {
		// Intent intent = new Intent();
		//
		// intent.setComponent(new ComponentName("com.android.calendar",
		// "com.android.calendar.LaunchActivity"));
		// startActivity(intent);
		// } catch (Exception e) {
		// Toast.makeText(getActivity(), R.string.toast_google_calendar,
		// Toast.LENGTH_LONG).show();
		// }
	}

	@Override
	public void onGetNewArguments(Bundle bn) {

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void initFragmentList(List<Fragment> listFragment) {
		actItem = (ActivityItem) getActivity().getIntent()
				.getSerializableExtra("item");

		listFragment.add(new DetailInfoFragment(
				getString(R.tag.fragment_detail_info),
				getString(R.string.menu_activity)));

		if ((actItem.location != null) && (!actItem.location.equals(""))) {
			listFragment.add(new DetailMapFragment(
					getString(R.tag.fragment_detail_map),
					getString(R.string.menu_map)));
		}

		listFragment.add(new DetailWebFragment(
				getString(R.tag.fragment_detail_web),
				getString(R.string.menu_url)));

	}

}
