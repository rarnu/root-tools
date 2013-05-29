package com.zoe.calendar.fragment;

import java.util.List;

import android.app.Fragment;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Toast;

import com.rarnu.devlib.base.BaseTabFragment;
import com.zoe.calendar.R;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.classes.GoogleCalendar;
import com.zoe.calendar.common.MenuIds;
import com.zoe.calendar.dialog.EventDialog;
import com.zoe.calendar.utils.GoogleCalendarUtils;

public class DetailFragment extends BaseTabFragment {

	MenuItem miCalendar, miShare;
	GoogleCalendar gc;
	ActivityItem actItem;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		actItem = (ActivityItem) getActivity().getIntent()
				.getSerializableExtra("item");
		gc = GoogleCalendarUtils.getCalendars(getActivity());
	}

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
	public void initMenu(Menu menu) {
		miCalendar = menu.add(0, MenuIds.MENU_CALENDAR, 23,
				R.string.menu_calendar);
		if (gc != null
				&& GoogleCalendarUtils.eventExists(getActivity(), gc, actItem)) {
			miCalendar.setIcon(R.drawable.ic_calendar_cancel);
		} else {
			miCalendar.setIcon(R.drawable.ic_calendar);
		}
		miCalendar.setShowAsAction(MenuItem.SHOW_AS_ACTION_ALWAYS);

		miShare = menu.add(0, MenuIds.MENU_SHARE, 24, R.string.menu_share);
		miShare.setIcon(R.drawable.ic_share_dropdown);
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
				miCalendar.setIcon(R.drawable.ic_calendar);
				startCalendar(true);
			} else {
				GoogleCalendarUtils.addEvent(getActivity(), gc, actItem);
				miCalendar.setIcon(R.drawable.ic_calendar_cancel);
				startCalendar(false);
			}
			break;
		case MenuIds.MENU_SHARE:
			// TODO: share event
			break;
		}
		return super.onOptionsItemSelected(item);
	}

	private void startCalendar(boolean delete) {
		// hint add or delete event
		startActivity(new Intent(getActivity(), EventDialog.class).putExtra(
				"deleted", delete));
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
