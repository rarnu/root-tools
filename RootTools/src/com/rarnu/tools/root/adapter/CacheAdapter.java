package com.rarnu.tools.root.adapter;

import android.content.Context;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;
import com.rarnu.devlib.base.adapter.BaseAdapter;
import com.rarnu.tools.root.GlobalInstance;
import com.rarnu.tools.root.R;
import com.rarnu.tools.root.common.CacheInfo;
import com.rarnu.tools.root.holder.CacheAdapterHolder;

import java.util.List;

public class CacheAdapter extends BaseAdapter<CacheInfo> {

    public CacheAdapter(Context context, List<CacheInfo> list) {
        super(context, list);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View v = convertView;
        if (v == null) {
            v = inflater.inflate(R.layout.cache_item, parent, false);
        }
        CacheAdapterHolder holder = (CacheAdapterHolder) v.getTag();
        if (holder == null) {
            holder = new CacheAdapterHolder();
            holder.imgIcon = (ImageView) v.findViewById(R.id.item_icon);
            holder.tvName = (TextView) v.findViewById(R.id.item_name);
            holder.tvPath = (TextView) v.findViewById(R.id.item_path);
            holder.tvCache = (TextView) v.findViewById(R.id.item_cache);
            v.setTag(holder);
        }
        CacheInfo item = list.get(position);
        if (item != null) {
            holder.imgIcon.setBackgroundDrawable(GlobalInstance.pm.getApplicationIcon(item.info.applicationInfo));
            holder.tvName.setText(GlobalInstance.pm.getApplicationLabel(item.info.applicationInfo));
            holder.tvPath.setText(item.info.applicationInfo.sourceDir);
            holder.tvCache.setText(item.cacheSize);
        }
        return v;
    }

    @Override
    public String getValueText(CacheInfo item) {
        return GlobalInstance.pm.getApplicationLabel(item.info.applicationInfo).toString() + item.info.packageName;
    }
}
