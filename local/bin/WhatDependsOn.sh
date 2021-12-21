#!/bin/bash
psql -h pgdev.activestate.build -U pb_admin inventory_pr_$AS_PLATFORM \
    > /tmp/wdo-$$ << EOF
select
    ins.namespace,
    i.name,
    dns.namespace,
    d.feature
from
    dependency as d
join
    namespace as dns
using
    (namespace_id)
join
    ingredient_version_revision as ivr
using
    (ingredient_version_revision_id)
join
    ingredient_version as iv
using
    (ingredient_version_id)
join
    ingredient as i
using
    (ingredient_id)
join
    namespace as ins
on
    ins.namespace_id = i.primary_namespace_id
where
    feature = '$1'
;
EOF


head -2 /tmp/wdo-$$
tail +2 /tmp/wdo-$$ | head -n -2 | sort -u
tail -2 /tmp/wdo-$$