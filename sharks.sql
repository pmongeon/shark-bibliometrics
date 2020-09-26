drop table #data
select distinct *
into #data
from wos_2013_text..text_data
where CONTAINS(title, 'shark')
union select *
from wos_2013_text..text_data
where CONTAINS(title, 'sharks')
union select *
from wos_2013_text..text_data
where CONTAINS(keyword, 'shark')
union 
select *
from wos_2013_text..text_data
where CONTAINS(keyword, 'sharks')
union select *
from wos_2013_text..text_data
where CONTAINS(keyword_plus, 'shark')
union 
select *
from wos_2013_text..text_data
where CONTAINS(keyword_plus, 'sharks')
union select *
from wos_2013_text..text_data
where CONTAINS(abstract, 'shark')
union 
select *
from wos_2013_text..text_data
where CONTAINS(abstract, 'sharks')

select *
into userdb_mongeonp2..shark_ut
from #data a

select b.*, a.abstract, a.keyword, a.keyword_plus, c.cluster_id1, c.cluster_id2, c.cluster_id3, f.EDiscipline as nsf_discipline, f.eSpecialite nsf_specialty
from userdb_mongeonp2..shark_ut a
join wos_2013..pub_detail b on b.ut = a.ut
left join wos_2013_classification..clustering c on c.ut = b.ut
left join userdb_mongeonp2..ost_ut_code_revue d on d.ut = b.ut
left join userdb_mongeonp2..ost_liste_revue e on e.Code_Revue = d.code_revue
left join userdb_mongeonp2..ost_liste_discipline f on f.Code_Discipline = e.Code_Discipline

select a.ut, c.noun_phrase, b.freq, 'title' as [source]
from userdb_mongeonp2..shark_ut a
join wos_2013_text..title_noun_phrase b on b.ut = a.ut
join wos_2013_text..noun_phrase c on c.noun_phrase_id = b.noun_phrase_id
union select a.ut, c.noun_phrase, b.freq, 'abstract' as [source]
from userdb_mongeonp2..shark_ut a
join wos_2013_text..abstract_noun_phrase b on b.ut = a.ut
join wos_2013_text..noun_phrase c on c.noun_phrase_id = b.noun_phrase_id

select b.citing_ut, b.cited_ut
from userdb_mongeonp2..shark_ut a
join wos_2013..citation b on b.citing_ut = a.ut
union select b.citing_ut, b.cited_ut
from userdb_mongeonp2..shark_ut a
join wos_2013..citation b on b.cited_ut = a.ut







