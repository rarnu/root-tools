package com.rarnu.vim.emotion;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.View;

import com.rarnu.vim.emotion.common.FaceLoader;
import com.rarnu.vim.emotion.comp.HScrollLayout;
import com.rarnu.vim.emotion.comp.OnScreenChangeListener;
import com.rarnu.vim.emotion.comp.OnScreenTouchListener;
import com.rarnu.vim.emotion.comp.ScrollLayout;
import com.rarnu.vim.emotion.database.EmotionDatabase;
import com.rarnu.vim.emotion.fragment.BottomFragment;
import com.rarnu.vim.emotion.fragment.CenterFragment;
import com.rarnu.vim.emotion.fragment.LeftFragment;
import com.rarnu.vim.emotion.fragment.TopFragment;
import com.rarnu.vim.emotion.utils.FacePageUtils;
import com.rarnu.vim.emotion.utils.SharePageUtils;
import com.rarnu.vim.emotion.utils.UIUtils;

public class MainActivity extends Activity implements OnScreenChangeListener,
		EmotionInterface, OnScreenTouchListener {

	ScrollLayout slLeftRight;
	HScrollLayout slTopBottom;

	TopFragment top;
	BottomFragment bottom;
	CenterFragment center;
	LeftFragment left;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		UIUtils.initDisplayMetrics(getWindowManager());
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		slLeftRight = (ScrollLayout) findViewById(R.id.slLeftRight);
		slTopBottom = (HScrollLayout) findViewById(R.id.slTopBottom);

		slLeftRight.setOnScreenChangeListener(this);
		slTopBottom.setOnScreenChangeListener(this);
		slLeftRight.setOnScreenTouchListener(this);
		slTopBottom.setOnScreenTouchListener(this);
		slLeftRight.setToScreen(1);
		slTopBottom.setToScreen(1);

		Global.database = new EmotionDatabase(this);
		FaceLoader.loadFace(this);
		Global.face = FaceLoader.faceList.get(0);

		initFragment();
		getFragmentManager().beginTransaction()
				.replace(R.id.fragCenter, center).replace(R.id.fragLeft, left)
				.replace(R.id.fragTop, top).replace(R.id.fragBottom, bottom)
				.commit();

	}

	@Override
	protected void onDestroy() {
		Global.database.close();
		super.onDestroy();
	}

	private void initFragment() {
		top = new TopFragment(new FacePageUtils(this));
		bottom = new BottomFragment(new SharePageUtils(this));
		center = new CenterFragment();
		left = new LeftFragment();
	}

	@Override
	public void onScreenChange(View v, int screen) {
		switch (v.getId()) {
		case R.id.slLeftRight:
			slTopBottom.setToScreen(slTopBottom.getCurScreen());
			break;
		case R.id.slTopBottom:
			slLeftRight.setEnableScroll(slTopBottom.getCurScreen() == 1);
			top.setScrollable(slTopBottom.getCurScreen() == 0);
			top.snap();
			bottom.setScrollable(slTopBottom.getCurScreen() == 2);
			bottom.snap();
			break;
		}
	}

	@Override
	public void setCurrentFace(String faceName, String comment) {

		center.setCurrentFace(faceName);
		slTopBottom.snapToScreen(1);
		Global.database.insertEmotion(System.currentTimeMillis(), faceName,
				comment);
		left.reload();
	}

	@Override
	public void setTopBottomScrollable(boolean enable) {
		slTopBottom.setEnableScroll(enable);
	}

	@Override
	public void setLeftRightScrollable(boolean enable) {
		slLeftRight.setEnableScroll(enable);
	}

	@Override
	public void onActionScrolling(View v) {
		Log.e(getClass().getName(), "onActionScrolling");
		switch (v.getId()) {
		case R.id.slLeftRight:
			break;
		case R.id.slTopBottom:
			Log.e(getClass().getName(), "slLeftRight.setEnableScroll(false)");
			slLeftRight.setEnableScroll(false);
			break;
		}
	}

	@Override
	public void onActionReset(View v) {
		switch (v.getId()) {
		case R.id.slLeftRight:
			break;
		case R.id.slTopBottom:
			Log.e(getClass().getName(), "slLeftRight.setEnableScroll(true)");
			slLeftRight.setEnableScroll(slTopBottom.getCurScreen() == 1);
			top.setScrollable(slTopBottom.getCurScreen() == 0);
			top.snap();
			bottom.setScrollable(slTopBottom.getCurScreen() == 2);
			bottom.snap();
			break;
		}
		
	}
}
