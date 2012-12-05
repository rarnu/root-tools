package com.rarnu.zoe.love2;

import java.util.List;

import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.GridView;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;

import com.rarnu.zoe.love2.adapter.HistoryAdapter;
import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.DataInfo;
import com.rarnu.zoe.love2.common.DayInfo;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.utils.UIUtils;

public class HistoryActivity extends BaseActivity implements OnClickListener {

	RelativeLayout layTree;
	TextView tvTree;
	TextView tvValue5, tvValue4, tvValue3, tvValue2, tvValue1;
	GridView gvHistory;

	HistoryAdapter adapter = null;
	List<DataInfo> listHistory = null;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		buildTree(Global.database.getDay());

	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_history);
	}

	@Override
	protected void initComponents() {
		super.initComponents();
		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.history_record);
		title.getBarItem(Title.BARITEM_LEFT).setIcon(R.drawable.home);

		layTree = (RelativeLayout) findViewById(R.id.layTree);
		tvTree = (TextView) findViewById(R.id.tvTree);
		tvValue5 = (TextView) findViewById(R.id.tvValue5);
		tvValue4 = (TextView) findViewById(R.id.tvValue4);
		tvValue3 = (TextView) findViewById(R.id.tvValue3);
		tvValue2 = (TextView) findViewById(R.id.tvValue2);
		tvValue1 = (TextView) findViewById(R.id.tvValue1);
		gvHistory = (GridView) findViewById(R.id.gvHistory);

	}

	@Override
	protected void initEvents() {
		super.initEvents();
		title.getBarItem(Title.BARITEM_LEFT).setOnButtonClick(this);
	}

	@Override
	public void onClick(View v) {
		switch (v.getId()) {
		case Title.ITEM_LEFT:
			finish();
			break;
		}

	}

	private void buildTree(int day) {
		// day = 21;
		int height = UIUtils.getHeight() - UIUtils.dipToPx(64)
				- UIUtils.getStatusbarHeight(this);
		height = height / 21;

		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) tvTree
				.getLayoutParams();
		rlp.height = height * day;
		tvTree.setLayoutParams(rlp);

		for (int i = 0; i < day; i++) {
			ImageView ivLeaf = new ImageView(this);

			RelativeLayout.LayoutParams leafLp = new RelativeLayout.LayoutParams(
					LayoutParams.MATCH_PARENT, (int) (height * 2 / 3));
			leafLp.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM,
					RelativeLayout.TRUE);

			leafLp.addRule(i % 2 == 0 ? RelativeLayout.RIGHT_OF
					: RelativeLayout.LEFT_OF, R.id.tvTree);
			ivLeaf.setBackgroundResource(i % 2 == 0 ? R.drawable.history_leaf_right
					: R.drawable.history_leaf_left);
			leafLp.bottomMargin = (height * i) + (height / 3);
			ivLeaf.setLayoutParams(leafLp);
			layTree.addView(ivLeaf);
		}
	}

	@Override
	public void onWindowFocusChanged(boolean hasFocus) {
		super.onWindowFocusChanged(hasFocus);
		buildGrid(Global.database.getDay());
		buildValue(Global.database.getDay());
	}

	private void buildGrid(int day) {

		int height = gvHistory.getHeight() - UIUtils.dipToPx(28);
		height = height / 7;

		listHistory = Global.database.queryHistory();
		adapter = new HistoryAdapter(this, listHistory, height);
		gvHistory.setAdapter(adapter);
		gvHistory.setEnabled(false);
	}

	private void buildValue(int day) {
		// TODO:
		int width = tvValue1.getWidth();
		width = width / 21;

		List<DayInfo> listInfo = Global.database.queryFullHistory();
		int task = 0, active = 0, food = 0, reading = 0, news = 0;
		for (int i = 0; i < listInfo.size(); i++) {
			task += (listInfo.get(i).task == 0 ? 1 : 0);
			active += (listInfo.get(i).active == 0 ? 1 : 0);
			food += (listInfo.get(i).food == 0 ? 1 : 0);
			reading += (listInfo.get(i).reading == 0 ? 1 : 0);
			news += (listInfo.get(i).news == 0 ? 1 : 0);
		}

		RelativeLayout.LayoutParams rlp = (RelativeLayout.LayoutParams) tvValue1
				.getLayoutParams();
		rlp.width = width * task;
		tvValue1.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) tvValue2.getLayoutParams();
		rlp.width = width * active;
		tvValue2.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) tvValue3.getLayoutParams();
		rlp.width = width * food;
		tvValue3.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) tvValue4.getLayoutParams();
		rlp.width = width * reading;
		tvValue4.setLayoutParams(rlp);

		rlp = (RelativeLayout.LayoutParams) tvValue5.getLayoutParams();
		rlp.width = width * news;
		tvValue5.setLayoutParams(rlp);
	}
}
