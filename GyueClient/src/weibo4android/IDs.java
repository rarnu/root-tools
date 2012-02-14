/*
Copyright (c) 2007-2009, Yusuke Yamamoto
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Yusuke Yamamoto nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY Yusuke Yamamoto ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL Yusuke Yamamoto BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
package weibo4android;

import java.util.Arrays;

import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import weibo4android.http.Response;
import weibo4android.org.json.JSONArray;
import weibo4android.org.json.JSONException;
import weibo4android.org.json.JSONObject;

/**
 * A data class representing array of numeric IDs.
 *
 * @author Yusuke Yamamoto - yusuke at mac.com
 * @editor sinaWeibo
 */
public class IDs extends WeiboResponse {
    private long[] ids;
    private long previousCursor;
    private long nextCursor;
    private static final long serialVersionUID = -6585026560164704953L;
    private static String[] ROOT_NODE_NAMES = {"id_list", "ids"};

    /*package*/ IDs(Response res) throws WeiboException {
        super(res);
        Element elem = res.asDocument().getDocumentElement();
        ensureRootNodeNameIs(ROOT_NODE_NAMES, elem);
        NodeList idlist = elem.getElementsByTagName("id");
        ids = new long[idlist.getLength()];
        for (int i = 0; i < idlist.getLength(); i++) {
            try {
                ids[i] = Long.parseLong(idlist.item(i).getFirstChild().getNodeValue());
            } catch (NumberFormatException nfe) {
                throw new WeiboException("Weibo API returned malformed response: " + elem, nfe);
            }
        }
        previousCursor = getChildLong("previous_cursor", elem);
        nextCursor = getChildLong("next_cursor", elem);
    }

    /*package*/ IDs(Response res,Weibo w) throws WeiboException {
        super(res);
        if("[]\n".equals(res.asString())){
        	previousCursor=0;
        	nextCursor=0;
        	ids= new long[0];
        	return;
        }
        JSONObject json=  res.asJSONObject();
        try {
        	previousCursor = json.getLong("previous_cursor");
            nextCursor = json.getLong("next_cursor");
        	
            if(!json.isNull("ids")){
        		JSONArray jsona= json.getJSONArray("ids");
        		int size=jsona.length();
        		ids =new long[ size];
        		for (int i = 0; i < size; i++) {
        			ids[i] =jsona.getLong(i);
				}
        	}
        	
         } catch (JSONException jsone) {
             throw new WeiboException(jsone);
         } 
        
    }

    public long[] getIDs() {
        return ids;
    }

    /**
     *
     * @since Weibo4J 1.2.0
     */
    public boolean hasPrevious(){
        return 0 != previousCursor;
    }

    /**
     *
     * @since Weibo4J 1.2.0
     */
    public long getPreviousCursor() {
        return previousCursor;
    }

    /**
     *
     * @since Weibo4J 1.2.0
     */
    public boolean hasNext(){
        return 0 != nextCursor;
    }

    /**
     *
     * @since Weibo4J 1.2.0
     */
    public long getNextCursor() {
        return nextCursor;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof IDs)) return false;

        IDs iDs = (IDs) o;

        if (!Arrays.equals(ids, iDs.ids)) return false;

        return true;
    }

    @Override
    public int hashCode() {
        return ids != null ? Arrays.hashCode(ids) : 0;
    }

    @Override
    public String toString() {
        return "IDs{" +
                "ids=" + ids +
                ", previousCursor=" + previousCursor +
                ", nextCursor=" + nextCursor +
                '}';
    }
}