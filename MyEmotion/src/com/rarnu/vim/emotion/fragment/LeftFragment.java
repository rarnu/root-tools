package com.rarnu.vim.emotion.fragment;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import android.content.Loader;
import android.content.Loader.OnLoadCompleteListener;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.rarnu.vim.emotion.R;
import com.rarnu.vim.emotion.adapter.HistoryAdapter;
import com.rarnu.vim.emotion.common.base.BaseFragment;
import com.rarnu.vim.emotion.comp.calendar.CalendarDays;
import com.rarnu.vim.emotion.comp.calendar.CalendarView;
import com.rarnu.vim.emotion.comp.calendar.OnCalendarChange;
import com.rarnu.vim.emotion.database.EmotionInfo;
import com.rarnu.vim.emotion.loader.HistoryLoader;

public class LeftFragment extends BaseFragment implements
		OnLoadCompleteListener<List<EmotionInfo>>, OnItemClickListener,
		OnCalendarChange {

	protected TextView tvTitle;
	protected ListView lvHistory;
	protected RelativeLayout layoutCalendar;
	protected CalendarView cvHistory;
	protected HistoryLoader loader = null;
	protected HistoryAdapter adapter = null;
	protected List<EmotionInfo> list = new ArrayList<EmotionInfo>();
	protected List<CalendarDays> listDays = new ArrayList<CalendarDays>();

	@Override
	public void initComponents() {

		lvHistory = (ListView) innerView.findViewById(R.id.lvHistory);
		lvHistory.setOnTouchListener(this);
		adapter = new HistoryAdapter(getActivity(), list);
		lvHistory.setAdapter(adapter);

		layoutCalendar = (RelativeLayout) innerView
				.findViewById(R.id.layoutCalendar);
		tvTitle = (TextView) innerView.findViewById(R.id.tvTitle);
		cvHistory = (CalendarView) innerView.findViewById(R.id.cvHistory);
		cvHistory.setOnCalendarClick(this);
		cvHistory.SetOnCalendarChange(this);
		initCalendar();

		loader = new HistoryLoader(getActivity());
		loader.registerListener(0, this);

	}

	public void init() {
		loader.startLoading();
	}

	public void initCalendar() {
		CalendarDays day = new CalendarDays();
		listDays.add(day);
		for (int i = 0; i < 12; i++) {
			listDays.add(0, listDays.get(0).getPriorMonth());
		}
		for (int i = 0; i < 12; i++) {
			listDays.add(listDays.get(listDays.size() - 1).getNextMonth());
		}
		cvHistory.setDate(listDays);
	}

	@Override
	public void onLoadComplete(Loader<List<EmotionInfo>> loader,
			List<EmotionInfo> data) {
		list.clear();
		if (data != null) {
			list.addAll(data);
		}
		adapter.setNewData(list);
	}

	public void reload() {
		loader.startLoading();
	}

	

	@Override
	public int getFragmentLayout() {
		return R.layout.layout_left;
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position,
			long id) {
		layoutCalendar.setVisibility(View.GONE);
	}

	@Override
	public void calendarChanged(CalendarDays days) {
		tvTitle.setText(String.format("%d.%d", days.year, days.month + 1));
	}

	@Override
	public void doShrink() {
		if (lvHistory.getVisibility() == View.VISIBLE) {
			cvHistory.gotoCurrentMonth();
			Calendar c = Calendar.getInstance();
			tvTitle.setText(String.format("%d.%d", c.get(Calendar.YEAR),
					c.get(Calendar.MONTH) + 1));
			layoutCalendar.setVisibility(View.VISIBLE);
		}
		
	}

	@Override
	public void doExpand() {
		
		
	}
}
