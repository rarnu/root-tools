package com.zoe.calendar.fragment;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.devlib.component.DragListView;
import com.rarnu.devlib.component.intf.RemoveListener;
import com.rarnu.devlib.component.tools.DragController;
import com.rarnu.utils.UIUtils;
import com.zoe.calendar.DetailActivity;
import com.zoe.calendar.Global;
import com.zoe.calendar.R;
import com.zoe.calendar.adapter.ActivityAdapter;
import com.zoe.calendar.classes.ActivityItem;
import com.zoe.calendar.common.Actions;
import com.zoe.calendar.component.CalendarDays;
import com.zoe.calendar.component.CalendarView;
import com.zoe.calendar.component.CalendarView.OnCalendarChange;
import com.zoe.calendar.component.Day;
import com.zoe.calendar.component.DayClickListener;
import com.zoe.calendar.database.QueryUtils;

public class MainFragment extends BaseFragment implements OnCalendarChange,
		DayClickListener, RemoveListener, OnItemClickListener, OnClickListener {

	CalendarView vpCalendar;
	DragListView lvCalender;
	DragController mController;
	ActivityAdapter adapterActivity;
	List<ActivityItem> listActivity;
	ImageView ivTrash;

	List<CalendarDays> listDays;
	TextView tvDate;
	private int currentDayLines;

	Day pointedDay;

	public MainFragment(String tag) {
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
		vpCalendar = (CalendarView) innerView.findViewById(R.id.vpCalendar);
		lvCalender = (DragListView) innerView.findViewById(R.id.lvCalendar);
		tvDate = (TextView) innerView.findViewById(R.id.tvDate);
		ivTrash = (ImageView) innerView.findViewById(R.id.ivTrash);

		mController = new DragController(lvCalender);
		mController.setRemoveEnabled(true);
		mController.setSortEnabled(false);
		mController.setDragInitMode(DragController.ON_DRAG);
		mController.setRemoveMode(DragController.FLING_REMOVE);
		mController.setBackgroundColor(0x110099CC);
		lvCalender.setFloatViewManager(mController);
		lvCalender.setDragEnabled(true);
		listActivity = new ArrayList<ActivityItem>();
		adapterActivity = new ActivityAdapter(getActivity(), listActivity);
		lvCalender.setAdapter(adapterActivity);
	}

	@Override
	public void initEvents() {
		vpCalendar.SetOnCalendarChange(this);
		vpCalendar.setDayClickListener(this);
		lvCalender.setOnTouchListener(mController);
		lvCalender.setRemoveListener(this);
		lvCalender.setOnItemClickListener(this);
		ivTrash.setOnClickListener(this);
	}

	@Override
	public void initLogic() {
		initCalendar();
		initActivity();
	}

	@Override
	public void onResume() {
		super.onResume();
		initPointedDay(pointedDay);
	}

	private void initActivity() {
		Day currentDay = new Day();
		Calendar cToday = Calendar.getInstance();
		currentDay.year = cToday.get(Calendar.YEAR);
		currentDay.month = cToday.get(Calendar.MONTH);
		currentDay.day = cToday.get(Calendar.DAY_OF_MONTH);
		onDayClick(0, 0, currentDay);
	}

	private void initPointedDay(Day day) {
		if (day != null) {
			onDayClick(0, 0, day);
		}
	}

	private void initCalendar() {

		int itemWidth = UIUtils.getWidth() / 7;
		int itemHeight = itemWidth * 3 / 4;

		listDays = new ArrayList<CalendarDays>();
		CalendarDays day = new CalendarDays(getActivity());
		calendarChanged(day);
		for (int i = 0; i < 3; i++) {
			listDays.add(day);
			day = day.getNextMonth();
		}
		vpCalendar.setDate(listDays, itemHeight);
		vpCalendar.setToScreen(0);
	}

	private void resetCalendarHeight(int lines) {
		currentDayLines = lines;
		int itemWidth = UIUtils.getWidth() / 7;
		int itemHeight = itemWidth * 3 / 4;

		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) vpCalendar
				.getLayoutParams();
		rlp.height = itemHeight * lines;
		vpCalendar.setLayoutParams(rlp);
	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_main;
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
		initActivity();
	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

	@Override
	public void calendarChanged(CalendarDays days) {
		tvDate.setText(String.format("%d 年 %d 月", days.year, days.month + 1));
		if (currentDayLines != days.lines) {
			resetCalendarHeight(days.lines);
		}
	}

	@Override
	public void onDayClick(int monthIndex, int position, Day day) {
		pointedDay = day;
		List<ActivityItem> list = QueryUtils.queryActivity(getActivity(),
				Global.city_pinyin, day.year, day.month + 1, day.day, 1);
		listActivity.clear();
		if (list != null) {
			listActivity.addAll(list);
		}
		adapterActivity.setNewList(listActivity);

		List<ActivityItem> listRemoved = QueryUtils.queryActivity(
				getActivity(), Global.city_pinyin, day.year, day.month + 1,
				day.day, 0);
		ivTrash.setVisibility((listRemoved == null || listRemoved.size() == 0) ? View.GONE
				: View.VISIBLE);
		vpCalendar.setSelection(monthIndex);
	}

	@Override
	public void remove(int which) {
		ActivityItem item = listActivity.get(which);
		QueryUtils.deleteActivity(getActivity(), item._id);
		listActivity.remove(which);
		adapterActivity.notifyDataSetChanged();
		ivTrash.setVisibility(View.VISIBLE);
	}

	class DatabaseMsgReceiver extends BroadcastReceiver {

		@Override
		public void onReceive(Context context, Intent intent) {
			initActivity();
		}
	}

	private DatabaseMsgReceiver receiverDatabase = new DatabaseMsgReceiver();
	private IntentFilter filterDatabase = new IntentFilter(
			Actions.ACTION_LOAD_DATABASE_FINISH);

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		getActivity().registerReceiver(receiverDatabase, filterDatabase);
	}

	@Override
	public void onDestroy() {
		getActivity().unregisterReceiver(receiverDatabase);
		super.onDestroy();
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View v, int position, long id) {
		ActivityItem item = listActivity.get(position);
		Intent inDetail = new Intent(getActivity(), DetailActivity.class);
		inDetail.putExtra("item", item);
		startActivity(inDetail);

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.ivTrash:
			// TODO: restore deleted data
			break;
		}
		
	}
}
