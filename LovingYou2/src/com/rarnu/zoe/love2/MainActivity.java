package com.rarnu.zoe.love2;

import android.os.Bundle;
import android.widget.ImageView;
import android.widget.ImageView.ScaleType;

import com.rarnu.zoe.love2.base.BaseActivity;
import com.rarnu.zoe.love2.comp.BottomBar;
import com.rarnu.zoe.love2.comp.RarnuGrid;
import com.rarnu.zoe.love2.comp.Title;
import com.rarnu.zoe.loving.utils.UIUtils;

public class MainActivity extends BaseActivity {

	RarnuGrid grid;
	BottomBar bottom;
	ImageView[] ivMain, ivSub;
	

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(getWindowManager());
		super.onCreate(savedInstanceState);
	}

	@Override
	protected void setContentView() {
		setContentView(R.layout.activity_main);
	}

	@Override
	protected void initComponents() {
		super.initComponents();

		title.getBarItem(Title.BARITEM_CENTER).setIcon(R.drawable.ic_launcher);
		title.getBarItem(Title.BARITEM_CENTER).setText(R.string.all_task);

		grid = (RarnuGrid) findViewById(R.id.grid);
		bottom = (BottomBar) findViewById(R.id.bottom);
		
		bottom.setText(BottomBar.BUTTON_1, R.string.task);
		bottom.setText(BottomBar.BUTTON_2, R.string.record);
		bottom.setText(BottomBar.BUTTON_3, R.string.square);
		bottom.setText(BottomBar.BUTTON_4, R.string.settings);
		
		setImages();
	}

	private void setImages() {
		ivMain = new ImageView[5];
		for (int i=0; i<5; i++) {
			ivMain[i] = new ImageView(this);
			ivMain[i].setAdjustViewBounds(true);
			ivMain[i].setScaleType(ScaleType.CENTER_INSIDE);
			ivMain[i].setImageResource(R.drawable.sp1);
			grid.setMainView(i, ivMain[i]);
		}
		
		ivSub = new ImageView[16];
		for (int i=0; i<16; i++) {
			ivSub[i] = new ImageView(this);
			ivSub[i].setAdjustViewBounds(true);
			ivSub[i].setScaleType(ScaleType.CENTER_INSIDE);
			ivSub[i].setImageResource(R.drawable.sp1);
		}

		grid.setSubView(ivSub);
	}
}
