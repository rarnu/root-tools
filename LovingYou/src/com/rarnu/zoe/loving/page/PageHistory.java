package com.rarnu.zoe.loving.page;

import java.util.List;

import android.content.Context;
import android.content.Intent;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.zoe.loving.DayActivity;
import com.rarnu.zoe.loving.Global;
import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.adapter.HistoryAdapter;
import com.rarnu.zoe.loving.base.BasePage;
import com.rarnu.zoe.loving.common.DataInfo;
import com.rarnu.zoe.loving.event.OnHistoryClick;
import com.rarnu.zoe.loving.utils.UIUtils;

public class PageHistory extends BasePage implements OnHistoryClick {

	TextView tvHistory, tvDesc;
	ListView lvHistory;

	List<DataInfo> list = null;
	HistoryAdapter adapter = null;

	public PageHistory(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public PageHistory(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public PageHistory(Context context) {
		super(context);
	}

	@Override
	protected void requireRootLayoutId() {
		this.rootLayout = R.layout.page_history;
	}

	@Override
	protected void init() {
		tvHistory = (TextView) findViewById(R.id.tvHistory);
		tvDesc = (TextView) findViewById(R.id.tvDesc);
		lvHistory = (ListView) findViewById(R.id.lvHistory);

		tvHistory.setText(String.format(
				getContext().getString(R.string.last_record),
				Global.database.getDay()));
	}

	@Override
	public void load(String... param) {
		loadData();
	}

	@Override
	public void refresh() {

	}

	@Override
	public void delete(int index) {

	}

	private void loadData() {

		if (list != null) {
			list = null;
		}
		list = Global.database.queryHistory();
		if (adapter != null) {
			adapter = null;
		}
		int itemWidth = UIUtils.getWidth() - UIUtils.dipToPx(32);
		itemWidth = itemWidth / 3;

		// 48+80+32+36+6
		int itemHeight = UIUtils.getHeight()
				- UIUtils.getStatusbarHeight(getContext())
				- UIUtils.dipToPx(210);
		itemHeight = itemHeight / 7;

		adapter = new HistoryAdapter(getContext(), list, itemWidth, itemHeight,
				this);
		lvHistory.setAdapter(adapter);
	}

	@Override
	public void onClick(View v, int index) {
		// TODO:
		if (index <= Global.database.getDay()) {
			Intent inDay = new Intent(getContext(), DayActivity.class);
			inDay.putExtra("day", index);
			getContext().startActivity(inDay);
		}

	}
}
