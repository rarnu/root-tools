package com.rarnu.tools.root.adapter;

import java.util.List;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.SysappInfo;
import com.rarnu.tools.root.holder.SysappAdapterHolder;
import com.rarnu.utils.DrawableUtils;

public class SysappAdapter extends BaseAdapter<SysappInfo> {

	public SysappAdapter(Context context, List<SysappInfo> list) {
		super(context, list);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		SysappInfo item = list.get(position);
		View v;
		if (convertView == null) {
			v = inflater.inflate(R.layout.sysapp_item, parent, false);
		} else {
			v = convertView;
		}
		SysappAdapterHolder holder = (SysappAdapterHolder) v.getTag();
		if (holder == null) {
			holder = new SysappAdapterHolder();
			holder.icon = (ImageView) v.findViewById(R.id.item_icon);
			holder.name = (TextView) v.findViewById(R.id.item_name);
			holder.path = (TextView) v.findViewById(R.id.item_path);
			v.setTag(holder);
		}

		if (item != null) {
			holder.icon.setBackgroundDrawable(GlobalInstance.pm
					.getApplicationIcon(item.info));
			holder.name.setText(GlobalInstance.pm
					.getApplicationLabel(item.info));
			holder.path.setText(item.info.sourceDir);

			holder.name.setTextColor(DrawableUtils.getTextColorPrimary(context));

			switch (item.level) {
			case 0:
				holder.name.setTextColor(Color.RED);
				break;
			case 1:
				holder.name.setTextColor(Color.GREEN);
				break;
			case 2:
				holder.name.setTextColor(0xFF6495ED);
				break;
			}

		}

		return v;
	}

	@Override
	public String getValueText(SysappInfo item) {
		return GlobalInstance.pm.getApplicationLabel(item.info).toString()
				+ item.info.packageName;
	}
}
