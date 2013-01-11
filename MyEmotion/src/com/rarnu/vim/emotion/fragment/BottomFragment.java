package com.rarnu.vim.emotion.fragment;

import android.content.Intent;
import android.content.pm.ResolveInfo;
import android.view.View;

import com.rarnu.vim.emotion.Global;
import com.rarnu.vim.emotion.R;
import com.rarnu.vim.emotion.common.base.BaseTopBottomFragment;
import com.rarnu.vim.emotion.utils.SharePageUtils;

public class BottomFragment extends BaseTopBottomFragment<SharePageUtils> {
	public BottomFragment(SharePageUtils pageUtils) {
		super(pageUtils);
	}

	@Override
	public int getFragmentLayout() {
		return R.layout.layout_bottom;
	}

	@Override
	public void onKeywordClick(View v, Object data) {
		super.onKeywordClick(v, data);
		ResolveInfo info = (ResolveInfo) data;

		Intent shareIntent = new Intent(Intent.ACTION_SEND);
		shareIntent.setType("image/*");
		shareIntent.setClassName(info.activityInfo.packageName,
				info.activityInfo.name);
		shareIntent.putExtra(Intent.EXTRA_TEXT, Global.face.name);
//		shareIntent.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(file));
		startActivity(shareIntent);

	}
}
