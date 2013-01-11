package com.rarnu.vim.emotion.fragment;

import android.widget.ImageView;

import com.rarnu.vim.emotion.Global;
import com.rarnu.vim.emotion.R;
import com.rarnu.vim.emotion.common.base.BaseFragment;
import com.rarnu.vim.emotion.utils.MiscUtils;

public class CenterFragment extends BaseFragment {

	ImageView imgEmotion;

	@Override
	public int getFragmentLayout() {
		return R.layout.layout_center;
	}

	@Override
	public void initComponents() {
		imgEmotion = (ImageView) innerView.findViewById(R.id.imgEmotion);
	}

	@Override
	public void init() {
		setCurrentFace(Global.face.value);
	}

	public void setCurrentFace(String faceName) {
		imgEmotion.setBackgroundDrawable(MiscUtils.getBitmapByAssets(
				getActivity(), "face/" + faceName));
	}

}
