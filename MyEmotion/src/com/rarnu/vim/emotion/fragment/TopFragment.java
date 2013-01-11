package com.rarnu.vim.emotion.fragment;

import android.view.View;

import com.rarnu.vim.emotion.EmotionInterface;
import com.rarnu.vim.emotion.Global;
import com.rarnu.vim.emotion.R;
import com.rarnu.vim.emotion.common.EmotionFace;
import com.rarnu.vim.emotion.common.base.BaseTopBottomFragment;
import com.rarnu.vim.emotion.utils.FacePageUtils;

public class TopFragment extends BaseTopBottomFragment<FacePageUtils> {
	public TopFragment(FacePageUtils pageUtils) {
		super(pageUtils);
	}

	@Override
	public int getFragmentLayout() {
		return R.layout.layout_top;
	}

	@Override
	public void onKeywordClick(View v, Object data) {
		super.onKeywordClick(v, data);
		EmotionFace info = (EmotionFace) data;
		Global.face = info;
		((EmotionInterface) getActivity())
				.setCurrentFace(info.value, info.name);

	}

	
}
