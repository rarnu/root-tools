package com.sbbs.me.android.loader;

import java.util.List;

import android.content.Context;

import com.rarnu.devlib.base.BaseLoader;
import com.rarnu.utils.FileUtils;
import com.sbbs.me.android.api.SbbsMeAPI;
import com.sbbs.me.android.api.SbbsMeImage;
import com.sbbs.me.android.consts.PathDefine;

public class SbbsGalleryLoader extends BaseLoader<SbbsMeImage> {

	boolean refresh = false;

	public SbbsGalleryLoader(Context context) {
		super(context);
	}

	public void setRefresh(boolean refresh) {
		this.refresh = refresh;
	}

	public boolean isRefresh() {
		return refresh;
	}

	@SuppressWarnings("unchecked")
	@Override
	public List<SbbsMeImage> loadInBackground() {
		List<SbbsMeImage> list = null;
		if (refresh) {
			list = SbbsMeAPI.getImages();
			if (list == null) {
				list = (List<SbbsMeImage>) FileUtils
						.loadListFromFile(PathDefine.CACHE_GALLERY_LIST);
			} else {
				FileUtils.saveListToFile(list, PathDefine.CACHE_GALLERY_LIST);
			}
		} else {
			list = (List<SbbsMeImage>) FileUtils
					.loadListFromFile(PathDefine.CACHE_GALLERY_LIST);
			if (list == null || list.size() == 0) {
				refresh = true;
				list = SbbsMeAPI.getImages();
				if (list != null) {
					FileUtils.saveListToFile(list,
							PathDefine.CACHE_GALLERY_LIST);
				}
			}
		}
		return list;
	}

}
