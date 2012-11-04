package com.rarnu.zoe.loving.page;

import java.util.List;

import android.content.Context;
import android.graphics.Color;
import android.util.AttributeSet;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ListView;
import android.widget.TextView;

import com.rarnu.zoe.loving.Global;
import com.rarnu.zoe.loving.R;
import com.rarnu.zoe.loving.adapter.HistoryAdapter;
import com.rarnu.zoe.loving.base.BasePage;
import com.rarnu.zoe.loving.common.DataInfo;
import com.rarnu.zoe.loving.utils.UIUtils;

public class PageHistory extends BasePage implements OnClickListener {

	TextView tvHEmotion, tvHActive, tvHFood, tvHFriend, tvHNews;
	ListView lvHistory;

	List<DataInfo> list = null;
	HistoryAdapter adapter = null;
	int itemWidth = 0;
	int selTab = 1;

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
		tvHEmotion = (TextView) findViewById(R.id.tvHEmotion);
		tvHActive = (TextView) findViewById(R.id.tvHActive);
		tvHFood = (TextView) findViewById(R.id.tvHFood);
		tvHFriend = (TextView) findViewById(R.id.tvHFriend);
		tvHNews = (TextView) findViewById(R.id.tvHNews);

		lvHistory = (ListView) findViewById(R.id.lvHistory);
		initEvent();
	}

	private void initEvent() {
		tvHEmotion.setOnClickListener(this);
		tvHActive.setOnClickListener(this);
		tvHFood.setOnClickListener(this);
		tvHFriend.setOnClickListener(this);
		tvHNews.setOnClickListener(this);
	}

	@Override
	public void load(String... param) {
		int baseWidth = UIUtils.getWidth() - UIUtils.dipToPx(32)
				- UIUtils.dipToPx(60);
		itemWidth = baseWidth / 3;
		setSelTab(1);

	}

	@Override
	public void refresh() {

	}

	@Override
	public void delete(int index) {

	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case R.id.tvHEmotion:
			setSelTab(1);
			break;
		case R.id.tvHActive:
			setSelTab(2);
			break;
		case R.id.tvHFood:
			setSelTab(3);
			break;
		case R.id.tvHFriend:
			setSelTab(4);
			break;
		case R.id.tvHNews:
			setSelTab(5);
			break;
		}

	}

	private void setSelTab(int tab) {
		selTab = tab;
		String columnName = "";
		tvHEmotion.setTextColor(Color.WHITE);
		tvHActive.setTextColor(Color.WHITE);
		tvHFood.setTextColor(Color.WHITE);
		tvHFriend.setTextColor(Color.WHITE);
		tvHNews.setTextColor(Color.WHITE);

		switch (tab) {
		case 1:
			columnName = "emotion";
			tvHEmotion.setTextColor(0xff3399FF);
			break;
		case 2:
			columnName = "active";
			tvHActive.setTextColor(0xff3399FF);
			break;
		case 3:
			columnName = "food";
			tvHFood.setTextColor(0xff3399FF);
			break;
		case 4:
			columnName = "friend";
			tvHFriend.setTextColor(0xff3399FF);
			break;
		case 5:
			columnName = "news";
			tvHNews.setTextColor(0xff3399FF);
			break;
		}
		if (list != null) {
			list = null;
		}
		list = Global.database.queryHistory(columnName);
		if (adapter != null) {
			adapter = null;
		}
		adapter = new HistoryAdapter(getContext(), list, itemWidth);
		lvHistory.setAdapter(adapter);
	}
}
