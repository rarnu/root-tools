package com.rarnu.zoe.love2;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageView;
import android.widget.Toast;
import android.widget.ImageView.ScaleType;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.common.Config;
import com.rarnu.zoe.love2.common.Consts;
import com.rarnu.zoe.love2.comp.BottomBar;
import com.rarnu.zoe.love2.comp.RarnuGrid;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.love2.database.DatabaseHelper;
import com.rarnu.zoe.love2.utils.UIUtils;

public class MainActivity extends BaseActivity implements OnClickListener {

	RarnuGrid grid;
	BottomBar bottom;
	ImageView[] ivMain, ivSub;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(getWindowManager());

		if (Config.getFirstStart(this)) {
			Config.setFirstStart(this, false);
			startActivity2(SplashActivity.class);

		}

		Global.database = new DatabaseHelper(this);
		super.onCreate(savedInstanceState);
	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_main);
	}

	@Override
	protected void initComponents() {
		super.initComponents();

		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.all_task);

		grid = (RarnuGrid) findViewById(R.id.grid);
		bottom = (BottomBar) findViewById(R.id.bottom);

		bottom.setText(BottomBar.BUTTON_1, R.string.square);
		bottom.setText(BottomBar.BUTTON_2, R.string.record);
		bottom.setText(BottomBar.BUTTON_3, R.string.history);
		bottom.setText(BottomBar.BUTTON_4, R.string.settings);

		bottom.setIcon(BottomBar.BUTTON_1, R.drawable.task_b3);
		bottom.setIcon(BottomBar.BUTTON_2, R.drawable.task_b2);
		bottom.setIcon(BottomBar.BUTTON_3, R.drawable.task);
		bottom.setIcon(BottomBar.BUTTON_4, R.drawable.task_b4);

		setImages(Global.database.getDay());
	}

	@Override
	protected void initEvents() {
		super.initEvents();
		bottom.setOnButtonClick(this);
		title.getBarItem(Title.BARITEM_RIGHT).setOnButtonClick(this);
	}

	private void setImages(int day) {

		if (day > 21) {
			day = 21;
		}

		ivMain = new ImageView[5];
		ivSub = new ImageView[16];
		for (int i = 0; i < 5; i++) {
			ivMain[i] = new ImageView(this);
			ivMain[i].setAdjustViewBounds(true);
			ivMain[i].setScaleType(ScaleType.CENTER_INSIDE);
			grid.setMainView(i, ivMain[i]);
		}

		for (int i = 0; i < 16; i++) {
			ivSub[i] = new ImageView(this);
			ivSub[i].setAdjustViewBounds(true);
			ivSub[i].setScaleType(ScaleType.CENTER_INSIDE);
		}
		grid.setSubView(ivSub);

		ivMain[0].setImageResource(Consts.spImgs[day - 1]);
		grid.setMainTag(0, day - 1);

		if (day <= 5) {
			// day1 01234
			// day2 10234
			// day3 20134
			// day4 30124
			// day5 40123
			int idx = 0;
			for (int i = 1; i < 5; i++) {
				if (idx == (day - 1)) {
					idx++;
				}
				ivMain[i]
						.setImageResource(i > (day - 1) ? R.drawable.task_not_reach
								: Consts.spImgs[idx]);
				grid.setMainTag(i, i > (day - 1) ? -1 : idx);
				idx++;
			}
			for (int i = 0; i < 16; i++) {
				ivSub[i].setImageResource(R.drawable.task_not_reach);
				grid.setSubTag(i, -1);

			}
		} else {
			for (int i = 1; i < 5; i++) {
				ivMain[i].setImageResource(Consts.spImgs[i - 1]);
				grid.setMainTag(i, i - 1);
			}

			int idx = 4;

			// day6 5 0123 4678
			// day7 6 0123 4578
			// day8 7 0123 4568
			for (int i = 0; i < 16; i++) {
				if (idx == (day - 1)) {
					idx++;
				}
				ivSub[i].setImageResource((i + 5) > (day - 1) ? R.drawable.task_not_reach
						: Consts.spImgs[idx]);
				grid.setSubTag(i, (i + 5) > (day - 1) ? -1 : idx);

				idx++;
			}
		}

		grid.setOnItemClickListener(this);

	}

	@Override
	public void onClick(View v) {

		Integer tag = (Integer) v.getTag();
		if (tag != null) {
			if (tag == -1) {
				Toast.makeText(this, R.string.not_arrived, Toast.LENGTH_LONG)
						.show();
				return;
			}
			Intent inTodo = new Intent(this, TodoActivity.class);
			inTodo.putExtra("index", tag);
			startActivity(inTodo);
			return;

		}

		switch (v.getId()) {

		case R.id.btn1:
			startActivity2(GroundActivity.class);
			break;
		case R.id.btn2:
			startActivity2(RecordActivity.class);
			break;
		case R.id.btn3:
			startActivity2(HistoryActivity.class);
			break;
		case R.id.btn4:
			startActivity2(SettingsActivity.class);
			break;
		}

	}

	private void startActivity2(Class<?> cls) {
		Intent inActivity = new Intent(this, cls);
		startActivity(inActivity);
	}
}
