package com.sbbs.me.android;

import android.app.Fragment;
import android.os.Bundle;
import android.view.MenuItem;

import com.rarnu.devlib.base.BaseSlidingActivity;
import com.rarnu.devlib.component.SlidingMenu;
import com.rarnu.utils.UIUtils;
import com.sbbs.me.android.fragment.ArticleFragment;
import com.sbbs.me.android.fragment.CommentFragment;

public class ArticleActivity extends BaseSlidingActivity {
	
	@Override
	public int getIcon() {
		return R.drawable.inner_logo;
	}

	@Override
	public Fragment replaceFragment() {
		Bundle bn = new Bundle();
		bn.putString("articleId", getIntent().getStringExtra("articleId"));
		bn.putSerializable("item", getIntent().getSerializableExtra("item"));
		ArticleFragment af = new ArticleFragment();
		af.setArguments(bn);
		return af;
	}

	@Override
	public void loadFragments() {
		
	}

	@Override
	public void releaseFragments() {
		
	}

	@Override
	public Fragment replaceMenuFragment() {
		return null; //new OriginPostFragment();
	}

	@Override
	public Fragment replaceSecondMenuFragment() {
		return new CommentFragment();
	}

	@Override
	public int getBehindOffset() {
		return UIUtils.dipToPx(100);
	}

	@Override
	public int getAboveTouchMode() {
		return SlidingMenu.TOUCHMODE_NONE;
	}

	@Override
	public int getBehindTouchMode() {
		return SlidingMenu.TOUCHMODE_MARGIN;
	}

	@Override
	public int getSlideMode() {
		return SlidingMenu.RIGHT;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		if (item.getItemId() == android.R.id.home) {
			finish();
			return true;
		}
		return super.onOptionsItemSelected(item);
	}
	
}
