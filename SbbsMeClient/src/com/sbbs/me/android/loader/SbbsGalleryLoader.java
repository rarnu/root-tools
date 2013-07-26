package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeImage;

public class SbbsGalleryLoader extends BaseLoader<SbbsMeImage> {

	public SbbsGalleryLoader(Context context) {
		super(context);
	}

	@Override
	public List<SbbsMeImage> loadInBackground() {
		List<SbbsMeImage> list = null;
		try {
			list = SbbsMeAPI.getImages();
		} catch (Exception e) {

		}
		return list;
	}

}
