package com.sbbs.me.android.fragment;

import android.os.Bundle;
import android.util.Log;
import android.view.Menu;

import com.rarnu.devlib.base.BaseFragment;
import com.rarnu.utils.ResourceUtils;
import com.sbbs.me.android.R;
import com.sbbs.me.android.api.SbbsMeBlock;

public class BlockFragment extends BaseFragment {

	String blockId;
	SbbsMeBlock block;
	
	public BlockFragment() {
		super();
		tagText = ResourceUtils.getString(R.tag.tag_block_fragment);
	}

	@Override
	public int getBarTitle() {
		return R.string.block_name;
	}

	@Override
	public int getBarTitleWithPath() {
		return R.string.block_name;
	}

	@Override
	public String getCustomTitle() {
		return null;
	}

	@Override
	public void initComponents() {

	}

	@Override
	public void initEvents() {
		blockId = getArguments().getString("blockId");
		block = (SbbsMeBlock) getArguments().getSerializable("item");
		Log.e("block", blockId);
	}

	@Override
	public void initLogic() {

	}

	@Override
	public int getFragmentLayoutResId() {
		return R.layout.fragment_block;
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

	}

	@Override
	public Bundle getFragmentState() {
		return null;
	}

}
