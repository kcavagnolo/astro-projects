;+
;========================================================================
;;;
;;; FILE NAME:    $Id: eb_common.pro,v 1.2 2008-10-15 19:35:51 cavagnolo Exp $
;;;
;;; DESCRIPTION:  Common Block for X-ray Event Browser application
;;;
;;; AUTHOR:       Pat Broos (patb@astro.psu.edu)
;;;               Scott Koch (tsk@astro.psu.edu)
;;;               Copyright (C) 1996, Pennsylvania State University
;;;
;;; NOTES:        See the document "X-ray Event Browser Software Design
;;;               Specification"
;;;
;;;               The name "dd" refers to domain dataset -- the var dd is a
;;;               structure containing the domain dataset.
;;;
;-
;==========================================================================
COMMON EB_PROPERTIES, param_list, prop_list, dds_size, $
		      wds_size, wds_mask, wds_indexes, null_data, epoch, $
		      primary_kywds, table_kywds, keywords_epoch, gui_active, eb_verbose
	

