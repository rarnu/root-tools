package com.rarnu.vim.emotion.common.base;

import java.util.List;

import android.view.View;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;

import com.rarnu.vim.emotion.EmotionInterface;
import com.rarnu.vim.emotion.R;
import com.rarnu.vim.emotion.common.PageItem;
import com.rarnu.vim.emotion.comp.GridPage4x4;
import com.rarnu.vim.emotion.comp.GridPage4x4.OnKeywordClickListener;
import com.rarnu.vim.emotion.comp.HScrollLayout;
import com.rarnu.vim.emotion.comp.OnScreenChangeListener;
import com.rarnu.vim.emotion.comp.OnScreenTouchListener;
import com.rarnu.vim.emotion.comp.PointBar;

public abstract class BaseTopBottomFragment<T extends BasePageUtils<?>> extends BaseFragment implements
		OnScreenChangeListener, OnKeywordClickListener, OnScreenTouchListener {

	protected HScrollLayout slButtons;
	protected PointBar pb;
	protected T pageUtils;
	
	public BaseTopBottomFragment(T pageUtils) {
		this.pageUtils = pageUtils;
	}
	
	@Override
	public void initComponents() {
		slButtons = (HScrollLayout) innerView.findViewById(R.id.slButtons);
		pb = (PointBar) innerView.findViewById(R.id.pb);
		slButtons.setOnScreenChangeListener(this);
		slButtons.setOnScreenTouchListener(this);
	}
	
	@Override
	public void onResume() {
		super.onResume();
		slButtons.snapToScreen(slButtons.getCurScreen());
	}
	
	@Override
	public void init() {
		initGrid9();
		pb.setPoint(0);
	}
	
	private void initGrid9() {
		slButtons.removeAllViews();
		List<PageItem[]> pages = pageUtils.buildPages();
		
		for (int i = 0; i < pages.size(); i++) {
			RelativeLayout lay = new RelativeLayout(getActivity());
			
			lay.setLayoutParams(new RelativeLayout.LayoutParams(
					LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
			slButtons.addView(lay);
			GridPage4x4 gp = new GridPage4x4(getActivity());
			
			RelativeLayout.LayoutParams lp = new RelativeLayout.LayoutParams(
					LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT);
			lp.addRule(RelativeLayout.CENTER_IN_PARENT, 1);
			gp.setLayoutParams(lp);
			gp.setButtonsItem(pages.get(i));
			gp.setButtonClickEvent(this);
			lay.addView(gp);
		}
		pb.setPointCount(slButtons.getChildCount());
	}

	@Override
	public void onKeywordClick(View v, Object data) {
		
		
	}

	@Override
	public void onScreenChange(View v, int screen) {
		pb.setPoint(screen);
	}
	
	public void setScrollable(boolean scrollable) {
		slButtons.setEnableScroll(scrollable);
	}
	
	public void snap() {
		slButtons.setToScreen(slButtons.getCurScreen());
	}
	
	@Override
	public void onActionScrolling(View v) {
		((EmotionInterface) getActivity()).setTopBottomScrollable(false);
		
	}

	@Override
	public void onActionReset(View v) {
		((EmotionInterface) getActivity()).setTopBottomScrollable(true);
		
	}
	
	@Override
	public void doShrink() {

	}

	@Override
	public void doExpand() {
		
		
	}
}
