package com.rarnu.devlib.intf;

import java.util.HashMap;

import android.content.Context;
import android.media.MediaPlayer;
import android.view.View;
import com.rarnu.devlib.base.PullToRefreshBase;

public class SoundPullEventListener<V extends View> implements PullToRefreshBase.OnPullEventListener<V> {

	private final Context mContext;
	private final HashMap<PullToRefreshBase.State, Integer> mSoundMap;

	private MediaPlayer mCurrentMediaPlayer;

	public SoundPullEventListener(Context context) {
		mContext = context;
		mSoundMap = new HashMap<PullToRefreshBase.State, Integer>();
	}

	@Override
	public final void onPullEvent(PullToRefreshBase<V> refreshView, PullToRefreshBase.State event, PullToRefreshBase.Mode direction) {
		Integer soundResIdObj = mSoundMap.get(event);
		if (null != soundResIdObj) {
			playSound(soundResIdObj.intValue());
		}
	}

	public void addSoundEvent(PullToRefreshBase.State event, int resId) {
		mSoundMap.put(event, resId);
	}

	public void clearSounds() {
		mSoundMap.clear();
	}

	public MediaPlayer getCurrentMediaPlayer() {
		return mCurrentMediaPlayer;
	}

	private void playSound(int resId) {
		if (null != mCurrentMediaPlayer) {
			mCurrentMediaPlayer.stop();
			mCurrentMediaPlayer.release();
		}

		mCurrentMediaPlayer = MediaPlayer.create(mContext, resId);
		if (null != mCurrentMediaPlayer) {
			mCurrentMediaPlayer.start();
		}
	}

}
